use nom::number::streaming::le_u32;
use nom::{multi::many0, IResult};
use std::io::BufReader;
use std::io::Read;

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
    pub dimensions: Dimensions,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Frame {
    pub time: f32,
    pub values: Vec<f32>,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Dimensions {
    pub i_min: u32,
    pub i_max: u32,
    pub j_min: u32,
    pub j_max: u32,
    pub k_min: u32,
    pub k_max: u32,
}

#[derive(Debug)]
pub struct SliceParser<R> {
    reader: BufReader<R>,
    pub header: SliceHeader,
    pub buf: Vec<u8>,
}

impl<R: Read> SliceParser<R> {
    pub fn new(mut input: R) -> Self {
        let mut read_buffer: Vec<u8> = vec![0; 8000];
        // parse_slice_file(&buf).unwrap();
        // let read = input.read(&mut read_buffer).unwrap();
        let (rem, header) = loop {
            let read = input.read(&mut read_buffer).unwrap();
            match parse_slice_header(&read_buffer) {
                Ok(header) => break header,
                Err(nom::Err::Incomplete(n)) => println!("Needed: {:?}", n),
                Err(nom::Err::Error(e)) => panic!("Error: {:?}", e.code),
                Err(nom::Err::Failure(e)) => panic!("Failure: {:?}", e.code),
            }
            if read == 0 {
                panic!("no header")
            }
        };
        let mut buf = Vec::new();
        buf.extend_from_slice(rem);
        SliceParser {
            reader: BufReader::new(input),
            header,
            buf,
        }
    }
}

impl<R: Read> Iterator for SliceParser<R> {
    type Item = Frame;

    fn next(&mut self) -> Option<Self::Item> {
        let (rem, frame) = loop {
            let i_dim = self.header.dimensions.i_max - self.header.dimensions.i_min + 1;
            let j_dim = self.header.dimensions.j_max - self.header.dimensions.j_min + 1;
            let k_dim = self.header.dimensions.k_max - self.header.dimensions.k_min + 1;
            // First we try and read a frame from what we have in the buffer.
            match parse_data_set(i_dim, j_dim, k_dim, &self.buf) {
                Ok(x) => {
                    break x;
                }
                Err(nom::Err::Incomplete(n)) => {
                    match n {
                        nom::Needed::Size(n) => self.buf.reserve(n.into()),
                        _ => panic!("extra buffer size not known"),
                    }
                    let l = self.buf.len();
                    let read = self.reader.read(&mut self.buf[l..]).unwrap();
                    if read == 0 {
                        panic!("no data frame")
                    }
                }
                Err(nom::Err::Error(e)) => panic!("Error: {:?}", e.code),
                Err(nom::Err::Failure(e)) => panic!("Failure: {:?}", e.code),
            }
        };
        let mut new_buf = Vec::new();
        new_buf.extend_from_slice(rem);
        self.buf = new_buf;
        Some(frame)
    }
}

pub fn parse_slice_file(i: &[u8]) -> IResult<&[u8], SliceFile> {
    let (i, header) = parse_slice_header(i)?;
    let i_dim = header.dimensions.i_max - header.dimensions.i_min + 1;
    let j_dim = header.dimensions.j_max - header.dimensions.j_min + 1;
    let k_dim = header.dimensions.k_max - header.dimensions.k_min + 1;
    let (i, frames) = many0(|iv| parse_data_set(i_dim, j_dim, k_dim, iv))(i)?;
    if i.is_empty() {
        Ok((i, SliceFile { header, frames }))
    } else {
        Err(nom::Err::Error(error_position!(
            i,
            nom::error::ErrorKind::RegexpMatch
        )))
    }
}

pub fn parse_data_set(i_dim: u32, j_dim: u32, k_dim: u32, i: &[u8]) -> IResult<&[u8], Frame> {
    let (i, rec_length) = nom::combinator::complete(le_u32)(i)?;
    let (i, time) = nom::number::streaming::le_f32(i)?;
    let (i, check) = le_u32(i)?;
    if check != rec_length {
        return Err(nom::Err::Failure(nom::error::make_error(
            i,
            nom::error::ErrorKind::Permutation,
        )));
    }
    let (i, values) = parse_slice_data(i_dim, j_dim, k_dim, i)?;
    Ok((i, Frame { time, values }))
}

pub fn parse_slice_data(i_dim: u32, j_dim: u32, k_dim: u32, i: &[u8]) -> IResult<&[u8], Vec<f32>> {
    let (i, rec_length) = le_u32(i)?;
    let (i, data) = nom::multi::count(
        nom::number::streaming::le_f32,
        (i_dim * j_dim * k_dim) as usize,
    )(i)?;
    let (i, check) = le_u32(i)?;
    if check != rec_length {
        return Err(nom::Err::Failure(nom::error::make_error(
            i,
            nom::error::ErrorKind::TakeWhileMN,
        )));
    }
    Ok((i, data))
}

fn parse_slice_header(i: &[u8]) -> IResult<&[u8], SliceHeader> {
    let (i, quantity) = parse_record(i)?;
    let (i, short_name) = parse_record(i)?;
    let (i, units) = parse_record(i)?;
    let (i, dimensions) = parse_dimensions(i)?;
    Ok((
        i,
        SliceHeader {
            quantity: nom::lib::std::str::from_utf8(quantity).unwrap().to_string(),
            short_name: nom::lib::std::str::from_utf8(short_name)
                .unwrap()
                .to_string(),
            units: nom::lib::std::str::from_utf8(units).unwrap().to_string(),
            dimensions,
        },
    ))
}

// #[derive(Clone)]
// pub enum ParseDimenionsError {
//     InvalidRecLength,
// }

// impl nom::error::ParseError for ParseDimenionsError {
//     fn from_error_kind(input: I, kind: ErrorKind) -> Self {

//     }

//     fn append(input: I, kind: ErrorKind, other: Self) -> Self {

//     }
// }

// impl std::fmt::Display for ParseDimenionsError {
// 	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
// 		write!(f, "A parsing error occurred.")
// 	}
// }
// impl std::fmt::Debug for ParseDimenionsError {
// 	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
// 		<ParseDimenionsError as std::fmt::Display>::fmt(self, f)
// 	}
// }
// impl std::error::Error for ParseDimenionsError { }

fn parse_dimensions(i: &[u8]) -> IResult<&[u8], Dimensions /*, ParseDimenionsError */> {
    // Take the length of the record, which is the first 4 bytes of the record
    // as a 32-bit as an integer. The length is in bytes.
    let (i, rec_length) = le_u32(i)?;
    if rec_length != 24 {
        return Err(nom::Err::Failure(nom::error::make_error(
            i,
            nom::error::ErrorKind::CrLf,
        )));
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
        return Err(nom::Err::Failure(nom::error::make_error(
            i,
            nom::error::ErrorKind::Fix,
        )));
    }
    Ok((
        i,
        Dimensions {
            i_min: i1,
            i_max: i2,
            j_min: j1,
            j_max: j2,
            k_min: k1,
            k_max: k2,
        },
    ))
}

/// Parse the data from a record, ensuring the record length tags at the start
/// and finish match.
fn parse_record(i: &[u8]) -> IResult<&[u8], &[u8]> {
    // Take the length of the record, which is the first 4 bytes of the record
    // as a 32-bit as an integer. The length is in bytes.
    let (i, rec_length) = le_u32(i)?;
    // Take the number of bytes specified by rec_length.
    let (i, b_string) = nom::bytes::streaming::take(rec_length)(i)?;
    let (i, check) = le_u32(i)?;
    if check != rec_length {
        panic!("bad rec_length start: {} end: {}", rec_length, check);
        return Err(nom::Err::Failure(nom::error::make_error(
            i,
            nom::error::ErrorKind::ManyMN,
        )));
    }
    Ok((i, b_string))
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
        assert_eq!(
            result.header.dimensions,
            Dimensions {
                i_min: 14,
                i_max: 14,
                j_min: 0,
                j_max: 10,
                k_min: 0,
                k_max: 24,
            }
        );
        assert_eq!(result.frames.len(), 945);
    }

    #[test]
    fn parse_slice_simple_bad01() {
        let result = parse_slice_file(include_bytes!("room_fire_01_bad01.sf"));
        assert_eq!(
            result.map_err(|e| match e {
                nom::Err::Failure(e) => e.code,
                _ => panic!("bad result"),
            }),
            Err(nom::error::ErrorKind::CrLf)
        );
    }
}
