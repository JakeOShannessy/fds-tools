use nom::{
    multi::{many0},
    IResult,
};
use nom::number::streaming::le_u32;

#[derive(Clone, Debug, PartialEq)]
pub struct SliceFile {
    pub header: SliceHeader,
    pub frames: Vec<Frame>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct SliceHeader {
    pub quantity: String,
    pub short_name: String,
    pub units: String,
    pub dimensions: (u32,u32,u32,u32,u32,u32),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Frame {
    pub time: f32,
    pub values: Vec<f32>,
}

pub fn parse_slice_file<'a, 'b>(i: &'b [u8]) -> IResult<&'b [u8], SliceFile> {
    let (i, header) = parse_slice_header(i)?;
    let i_dim = header.dimensions.1 - header.dimensions.0 + 1;
    let j_dim = header.dimensions.3 - header.dimensions.2 + 1;
    let k_dim = header.dimensions.5 - header.dimensions.4 + 1;
    let (i, frames) = many0(|iv| parse_data_set(i_dim, j_dim, k_dim, iv))(i)?;
    if i.len() == 0 {
        Ok((i, SliceFile {
            header,
            frames,
        }))
    } else {
        Err(nom::Err::Error(error_position!(
            i,
            nom::error::ErrorKind::Eof
        )))
    }

}

pub fn parse_data_set<'a, 'b>(i_dim: u32, j_dim: u32, k_dim: u32, i: &'b [u8]) -> IResult<&'b [u8], Frame> {

    let (i, rec_length) = nom::combinator::complete(le_u32)(i)?;
    let (i, time) = nom::number::streaming::le_f32(i)?;
    let (i, check) = le_u32(i)?;
    if check != rec_length {
        panic!("slice malformed");
    }
    let (i, values) = parse_slice_data(i_dim, j_dim, k_dim, i)?;
    Ok((i, Frame {
        time,
        values,
    }))
}

pub fn parse_slice_data<'a, 'b>(i_dim: u32, j_dim: u32, k_dim: u32, i: &'b [u8]) -> IResult<&'b [u8], Vec<f32>> {
    let (i, rec_length) = le_u32(i)?;
    let (i, data) = nom::multi::count(nom::number::streaming::le_f32, (i_dim*j_dim*k_dim) as usize)(i)?;
    let (i, check) = le_u32(i)?;
    if check != rec_length {
        panic!("slice malformed: start: {} end: {}, expected: {}", rec_length, check, i_dim*j_dim*k_dim);
    }
    Ok((i, data))
}

fn parse_slice_header(i: &[u8]) -> IResult<&[u8], SliceHeader> {
    let (i, quantity) = parse_record(i)?;
    let (i, short_name) = parse_record(i)?;
    let (i, units) = parse_record(i)?;
    let (i, dimensions) = parse_dimensions(i)?;
    Ok((i, SliceHeader {
        quantity: nom::lib::std::str::from_utf8(quantity).unwrap().to_string(),
        short_name: nom::lib::std::str::from_utf8(short_name).unwrap().to_string(),
        units: nom::lib::std::str::from_utf8(units).unwrap().to_string(),
        dimensions,
    }))
}

fn parse_dimensions(i: &[u8]) -> IResult<&[u8], (u32,u32,u32,u32,u32,u32)> {
    // Take the length of the record, which is the first 4 bytes of the record
    // as a 32-bit as an integer. The length is in bytes.
    let (i, rec_length) = le_u32(i)?;
    if rec_length != 24 {
        panic!("slice malformed");
    }
    // Take the number of bytes specified by rec_length.
    let (i, i1) = le_u32(i)?;
    let (i, i2) = le_u32(i)?;
    let (i, j1) = le_u32(i)?;
    let (i, j2) = le_u32(i)?;
    let (i, k1) = le_u32(i)?;
    let (i, k2) = le_u32(i)?;

    let (i, check) = le_u32(i)?;
    if check != rec_length {
        panic!("slice malformed");
    }
    Ok((i, (i1,i2,j1,j2,k1,k2)))
}

// -- |Parse the data from a record, ensuring the record length tags at the start
// -- and finish match.
fn parse_record(i: &[u8]) -> IResult<&[u8], &[u8]> {
    // Take the length of the record, which is the first 4 bytes of the record
    // as a 32-bit as an integer. The length is in bytes.
    let (i, rec_length) = le_u32(i)?;
    // Take the number of bytes specified by rec_length.
    let (i, b_string) = nom::bytes::streaming::take(rec_length)(i)?;
    let (i, check) = le_u32(i)?;
    if check != rec_length {
        panic!("slice malformed");
    }
    Ok((i, &b_string))
}

#[cfg(test)]
mod tests {
    use super::*;
    // In these tests Ok(remaining, result) is used to make sure that we have
    // consumed the input we expect to consume.
    #[test]
    fn parse_slice_simple() {
        let result = parse_slice_file(include_bytes!("room_fire_01.sf"))
            .expect("slice parsing failed")
            .1;
        assert_eq!(result.header.quantity.trim(), "TEMPERATURE".to_string());
        assert_eq!(result.header.units.trim(), "C".to_string());
        assert_eq!(result.header.short_name.trim(), "temp".to_string());
        assert_eq!(result.header.dimensions, (14,14,0,10,0,24));
        assert_eq!(result.frames.len(), 945);
    }
}
