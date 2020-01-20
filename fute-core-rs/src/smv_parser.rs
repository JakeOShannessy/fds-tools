#![allow(unused_imports)]
use nom::error::ErrorKind;
use nom::error::ParseError;
use nom::lib::std::ops::{Range, RangeFrom, RangeTo};
use nom::Err;
use nom::{
    branch::alt,
    bytes::complete::{is_a, tag},
    character::complete::{
        alphanumeric1, anychar, char, crlf, digit1, line_ending, multispace0, newline, none_of,
        not_line_ending, one_of, space0,
    },
    combinator::{opt, peek, value},
    multi::{many0, many1, many_till, separated_list},
    number::complete::double,
    sequence::preceded,
    IResult,
};
use nom::{AsChar, FindToken, InputIter, InputLength, InputTakeAtPosition, Slice};
use nom::{Compare, CompareResult};
use std::collections::HashMap;
use version_compare::version::Version;

#[derive(Clone, Debug, PartialEq)]
pub struct SMVFile {
    pub title: String,
    pub chid: String,
    pub csvfs: Vec<CSVEntry>,
    // , fds_version    : Version
    // , nMeshes       : u64
    // , surfs         : Vec<SMVSurf>
    // , pClass        : Vec<SMVPClass>
    // , outline       : Vec<SMVOutlineEntry>
    // , props         : Vec<SMVProp>
    // , devices       : Vec<SMVDevice>
    // , smvMeshes     : Vec<SMVMesh>
    // , smvDataFiles  : Vec<DataFileEntry>
    // , devcActs      : Vec<DevcActEntry>
    // , obstVis       : Vec<ObstVisEntry>
}

#[derive(Clone, Debug, PartialEq)]
pub struct RawSMVFile {
    title: Option<String>,
    chid: Option<String>,
    csvfs: Vec<CSVEntry>,
    // , fds_version    : Version
    // , nMeshes       : u64
    // , surfs         : Vec<SMVSurf>
    // , pClass        : Vec<SMVPClass>
    // , outline       : Vec<SMVOutlineEntry>
    // , props         : Vec<SMVProp>
    // , devices       : Vec<SMVDevice>
    // , smvMeshes     : Vec<SMVMesh>
    // , smvDataFiles  : Vec<DataFileEntry>
    // , devcActs      : Vec<DevcActEntry>
    // , obstVis       : Vec<ObstVisEntry>
    unknown_blocks: Vec<SMVBlock>,
}

impl RawSMVFile {
    pub fn new() -> Self {
        RawSMVFile {
            ..Default::default()
        }
    }
    pub fn add_block(&mut self, block: SMVBlock) {
        match block.title.as_str() {
            "TITLE" => {
                let s = block.content.trim();
                self.title = Some(s.to_string());
            }
            "CHID" => {
                let s = block.content.trim();
                self.chid = Some(s.to_string());
            }
            "CSVF" => {
                let (_i, s) = parse_csvf(&block.content).unwrap();
                self.csvfs.push(s);
            }
            _ => self.unknown_blocks.push(block),
        }
    }
}

impl Default for RawSMVFile {
    fn default() -> Self {
        RawSMVFile {
            title: Default::default(),
            chid: Default::default(),
            csvfs: Default::default(),
            unknown_blocks: Default::default(),
        }
    }
}

impl Into<SMVFile> for RawSMVFile {
    fn into(self) -> SMVFile {
        SMVFile {
            title: self.title.unwrap(),
            chid: self.chid.unwrap(),
            csvfs: self.csvfs,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct SMVBlock {
    pub title: String,
    pub content: String,
}

pub fn parse_smv_block<'a, 'b>(i: &'b str) -> IResult<&'b str, SMVBlock> {
    // Parse the title/category of the block
    let (i, title) = not_line_ending(i)?;
    let (i, _) = line_ending(i)?;
    // Include all lines that have a non-zero indentation within this block
    let (i, content) = not_zero_indent(i)?;
    Ok((
        i,
        SMVBlock {
            title: title.to_string(),
            content: content.to_string(),
        },
    ))
}

pub fn not_zero_indent<T, E: ParseError<T>>(input: T) -> IResult<T, T, E>
where
    T: Slice<Range<usize>> + Slice<RangeFrom<usize>> + Slice<RangeTo<usize>>,
    T: InputIter + InputLength,
    T: Copy,
    T: Compare<&'static str>,
    <T as InputIter>::Item: AsChar,
    <T as InputIter>::Item: AsChar,
{
    let mut temp_input = input;
    let mut total_index = 0;
    // loop through all the line endings until we find one that is preceded by non-whitespace
    // We don't need to worry about '\r' as that comes first
    loop {
        match temp_input.position(|item| {
            let c = item.as_char();
            c == '\n'
        }) {
            // There is no line ending for the remainder of the input
            None => {
                return Ok((input.slice(input.input_len()..), input));
            }
            Some(index) => {
                total_index += index + 1;
                let mut it = temp_input.slice(index..).iter_elements();
                // nth is the '\n' that we found
                let _nth = it.next().unwrap().as_char();
                // nth1 is one after
                let nth1 = match it.next() {
                    Some(n) => n.as_char(),
                    None => {
                        return Ok((input.slice(total_index..), input.slice(..total_index)));
                    }
                };
                // If the next value is whitespace, we want to update temp_input
                // and loop.
                if nth1.is_whitespace() {
                    temp_input = temp_input.slice((index + 1)..);
                    continue;
                } else {
                    return Ok((input.slice(total_index..), input.slice(..total_index)));
                }
            }
        }
    }
}

pub fn parse_smv_file<'a, 'b>(i: &'b str) -> IResult<&'b str, SMVFile> {
    let (i, blocks) = many0(parse_smv_block)(i)?;
    let mut raw_smv_file = RawSMVFile::new();
    for block in blocks {
        raw_smv_file.add_block(block);
    }
    if i.len() == 0 {
        Ok((i, raw_smv_file.into()))
    } else {
        Err(nom::Err::Error(error_position!(
            i,
            nom::error::ErrorKind::Eof
        )))
    }
}

// pub fn parse_smv_file_old<'a, 'b>(i: &'b str) -> IResult<&'b str, SMVFile> {
//     let (i, _) = tag("TITLE")(i)?;
//     let (i, _) = line_ending(i)?;
//     let (i, _) = char(' ')(i)?;
//     let (i, title) = not_line_ending(i)?;
//     let (i, _) = line_ending(i)?;
//     let (i, _) = line_ending(i)?;

//     let (i, _) = alt((tag("VERSION"), tag("FDSVERSION")))(i)?;
//     let (i, _) = line_ending(i)?;

//     let (i, _rev_str) = not_line_ending(i)?;
//     let (i, _) = line_ending(i)?;
//     let (i, _) = line_ending(i)?;

//     // TODO: This is only necessary in some versions
//     let (i, _) = tag("FDS")(i)?;
//     let (i, _) = char(' ')(i)?;
//     let (i, version_str) = not_line_ending(i)?;
//     let version = Version::from(version_str).unwrap();
//     let (i, _) = line_ending(i)?;
//     let (i, git_version) = not_line_ending(i)?;
//     let (i, _) = line_ending(i)?;
//     let (i, _) = line_ending(i)?;

//     let (i, _) = tag("ENDF")(i)?;
//     let (i, _) = line_ending(i)?;
//     let (i, _) = char(' ')(i)?;
//     let (i, _) = not_line_ending(i)?;
//     let (i, _) = line_ending(i)?;
//     let (i, _) = line_ending(i)?;

//     let (i, _) = tag("INPF")(i)?;
//     let (i, _) = line_ending(i)?;
//     let (i, _) = char(' ')(i)?;
//     let (i, _) = not_line_ending(i)?;
//     let (i, _) = line_ending(i)?;
//     let (i, _) = line_ending(i)?;

//     let (i, _) = tag("REVISION")(i)?;
//     let (i, _) = line_ending(i)?;
//     let (i, _) = not_line_ending(i)?;
//     let (i, _) = line_ending(i)?;
//     let (i, _) = line_ending(i)?;

//     let (i, _) = tag("CHID")(i)?;
//     let (i, _) = line_ending(i)?;
//     let (i, _) = char(' ')(i)?;
//     let (i, chid) = not_line_ending(i)?;
//     let (i, _) = line_ending(i)?;
//     let (i, _) = line_ending(i)?;

//     let (i, csvfs) = many0(parse_csvf)(i)?;

//     let (i, _) = tag("NMESHES")(i)?;
//     let (i, _) = line_ending(i)?;
//     let (i, _) = char(' ')(i)?;
//     let (i, n_meshes) = not_line_ending(i)?;
//     let (i, _) = line_ending(i)?;
//     let (i, _) = line_ending(i)?;

//     let (i, _) = tag("VIEWTIMES")(i)?;
//     let (i, _) = line_ending(i)?;
//     let (i, _) = space0(i)?;
//     let (i, a) = parse_double(i)?;
//     let (i, _) = space0(i)?;
//     let (i, b) = parse_double(i)?;
//     let (i, _) = space0(i)?;
//     let (i, c) = parse_int(i)?;
//     let (i, _) = line_ending(i)?;
//     let (i, _) = line_ending(i)?;
//     // TODO: it is important that this is implemented before production
//     // let (i, geom_model) = opt(parse_geom_model)(i)?;

//     let (i, _) = tag("ALBEDO")(i)?;
//     let (i, _) = line_ending(i)?;
//     let (i, _) = space0(i)?;
//     let (i, albedo) = parse_double(i)?;
//     let (i, _) = line_ending(i)?;
//     let (i, _) = line_ending(i)?;

//     let (i, iblank) = opt(parse_iblank)(i)?;
//     let (i, _) = parse_gvec(i)?;

//     // TODO: it is likely there is allowed to be multiple of these.
//     let (i, _) = tag("SURFDEF")(i)?;
//     let (i, _) = line_ending(i)?;
//     let (i, _) = space0(i)?;
//     let (i, surf_def) = not_line_ending(i)?;
//     let (i, _) = line_ending(i)?;
//     let (i, _) = line_ending(i)?;

//     let (i, surfs) = many0(parse_surface)(i)?;
//     let (i, materials) = many0(parse_material)(i)?;
//     let (i, geom) = opt(parse_geom)(i)?;
//     let (i, pclasses) = many0(parse_pclass)(i)?;
//     // let (i, hclasses) = many0(parse_hclass)(i)?;
//     let (i, outline) = parse_outline(i)?;

//     let (i, _) = tag("TOFFSET")(i)?;
//     let (i, _) = line_ending(i)?;
//     let (i, _) = space0(i)?;
//     let (i, e) = parse_double(i)?;
//     let (i, _) = space0(i)?;
//     let (i, f) = parse_double(i)?;
//     let (i, _) = space0(i)?;
//     let (i, g) = parse_double(i)?;
//     let (i, _) = line_ending(i)?;
//     let (i, _) = line_ending(i)?;

//     let (i, _) = tag("HRRPUVCUT")(i)?;
//     let (i, _) = line_ending(i)?;
//     let (i, _) = space0(i)?;
//     let (i, h) = parse_int(i)?;
//     let (i, _) = line_ending(i)?;
//     let (i, j) = many0(parse_hrrpuvcut_entry)(i)?;
//     let (i, _) = line_ending(i)?;
//     let (i, _) = line_ending(i)?;

//     let (i, _) = tag("RAMP")(i)?;
//     let (i, _) = line_ending(i)?;
//     let (i, _) = space0(i)?;
//     let (i, k) = parse_int(i)?;
//     let (i, _) = line_ending(i)?;
//     let (i, ramps) = nom::multi::count(parse_ramp, k as usize)(i)?;
//     let (i, _) = line_ending(i)?;

//     let (i, props) = many0(parse_prop)(i)?;
//     let (i, devices) = many0(parse_device)(i)?;

//     let (i, _) = tag("VERT")(i)?;
//     let (i, _) = line_ending(i)?;
//     let (i, _) = space0(i)?;
//     let (i, n_verts) = parse_int(i)?;
//     let (i, _) = line_ending(i)?;
//     let (i, verts) = nom::multi::count(parse_vert, n_verts as usize)(i)?;
//     let (i, _) = line_ending(i)?;

//     let (i, _) = tag("FACE")(i)?;
//     let (i, _) = line_ending(i)?;
//     let (i, _) = space0(i)?;
//     let (i, n_faces) = parse_int(i)?;
//     let (i, _) = line_ending(i)?;
//     let (i, faces) = nom::multi::count(parse_face, n_faces as usize)(i)?;
//     let (i, _) = line_ending(i)?;

//     let (i, meshes) = many0(parse_mesh)(i)?;
//     let (i, _) = opt(parse_smoke_diff)(i)?;
//     let (i, tail_entries) = many0(parse_tail_entry)(i)?;
//     let (dataFileEntries, devActEntries, obstVisEntries) = sort_tail_entries(tail_entries);

//     if i.len() == 0 {
//         Ok((
//             i,
//             SMVFile {
//                 title: title.to_string(),
//                 // fds_version: version,
//             },
//         ))
//     } else {
//         Err(nom::Err::Error(error_position!(
//             i,
//             nom::error::ErrorKind::Eof
//         )))
//     }
// }

fn parse_smoke_diff(i: &str) -> IResult<&str, ()> {
    let (i, _) = tag("SMOKEDIFF")(i)?;
    let (i, _) = line_ending(i)?;
    Ok((i, ()))
}

fn parse_hrrpuvcut_entry(i: &str) -> IResult<&str, f64> {
    let (i, _) = space0(i)?;
    let (i, n) = parse_double(i)?;
    Ok((i, n))
}

pub fn parse_int(i: &str) -> IResult<&str, i64> {
    let (i, s) = opt(alt((char('+'), char('-'))))(i)?;
    let (i, digits) = digit1(i)?;
    // allow a trailing decimal point and trailing zeros
    let (i, period) = opt(char('.'))(i)?;
    let i = if period.is_some() {
        many0(char('0'))(i)?.0
    } else {
        i
    };
    // TODO: fix this error handling
    let num = digits.parse::<i64>().expect("not a valid number");
    if s == Some('-') {
        Ok((i, -1 * num))
    } else {
        Ok((i, num))
    }
}

pub fn parse_uint(i: &str) -> IResult<&str, u64> {
    let (i, digits) = digit1(i)?;
    // TODO: fix this error handling
    let num = digits.parse::<u64>().expect("not a valid number");
    Ok((i, num))
}

pub fn parse_double(i: &str) -> IResult<&str, f64> {
    // must start with either a digit or a decimal point.
    peek(one_of("1234567890+-."))(i)?;
    double(i)
}

// pub fn parameter_name(i: &str) -> IResult<&str, String> {
//     let (i, _) = peek(none_of("&=/( \r\n\t0123456789.,"))(i)?;
//     let (i, p_name) = many1(none_of("=/( \r\n\t.,"))(i)?;
//     Ok((i, p_name.into_iter().collect()))
// }

// pub fn parse_nml_name(i: &str) -> IResult<&str, String> {
//     let (i, name) = alphanumeric1(i)?;
//     Ok((
//         i,
//         std::str::from_utf8(name)
//             .expect("namelist name is not valid utf8")
//             .to_string(),
//     ))
// }

// pub fn quoted_string(i: &str) -> IResult<&str, String> {
//     alt((quoted_string_single, quoted_string_double))(i)
// }

// pub fn quoted_string_single(i: &str) -> IResult<&str, String> {
//     let (i, _start_quote) = char('\'')(i)?;
//     let (i, s) = many0(none_of("\'"))(i)?;
//     let (i, _end_quote) = char('\'')(i)?;
//     Ok((i, s.iter().collect()))
// }

// pub fn quoted_string_double(i: &str) -> IResult<&str, String> {
//     let (i, _start_quote) = char('\"')(i)?;
//     let (i, s) = many0(none_of("\""))(i)?;
//     let (i, _end_quote) = char('\"')(i)?;
//     Ok((i, s.iter().collect()))
// }

#[derive(Clone, Debug, PartialEq)]
pub struct CSVEntry {
    pub type_: String,
    pub filename: String,
}

fn parse_csvf(i: &str) -> IResult<&str, CSVEntry> {
    let (i, _) = char(' ')(i)?;
    let (i, _) = space0(i)?;
    let (i, type_) = not_line_ending(i)?;
    let (i, _) = line_ending(i)?;
    let (i, _) = space0(i)?;
    let (i, filename) = not_line_ending(i)?;
    let (i, _) = line_ending(i)?;
    let (i, _) = line_ending(i)?;
    Ok((
        i,
        CSVEntry {
            type_: type_.to_string(),
            filename: filename.to_string(),
        },
    ))
}

fn parse_geom(i: &str) -> IResult<&str, String> {
    let (i, _) = space0(i)?;
    let (i, n) = parse_int(i)?;
    let (i, _) = line_ending(i)?;
    let (i, _) = space0(i)?;
    let (i, geom) = not_line_ending(i)?;
    let (i, _) = line_ending(i)?;
    Ok((i, geom.to_string()))
}

fn parse_ramp(i: &str) -> IResult<&str, Vec<(f64, f64)>> {
    let (i, _) = tag(" RAMP: ")(i)?;
    let (i, name_full) = not_line_ending(i)?;
    let name = name_full.trim();
    let (i, _) = line_ending(i)?;

    let (i, _) = space0(i)?;
    let (i, n_points) = parse_int(i)?;
    let (i, _) = line_ending(i)?;

    let (i, points) = nom::multi::count(parse_point, n_points as usize)(i)?;

    // TODO: return name as well
    Ok((i, points))
    // parseRamp :: Parser [(Double, Double)]
    // parseRamp = do
    //     _ <- string " RAMP: "
    //     name <- basicStringSpaces <* eol
    //     nPoints <- onlySpaces *> intNum <* eol
    //     points <- count nPoints parsePoint
    //     return points
    //     where
    //         -- parse t and f
    //         parsePoint = (,)
    //             <$> (onlySpaces *> floatNum)
    //             <*> (onlySpaces *> floatNum <* onlySpaces <* eol)
}

fn parse_point(i: &str) -> IResult<&str, (f64, f64)> {
    let (i, _) = space0(i)?;
    let (i, t) = parse_double(i)?;
    let (i, _) = space0(i)?;
    let (i, f) = parse_double(i)?;
    let (i, _) = space0(i)?;
    let (i, _) = line_ending(i)?;
    Ok((i, (t, f)))
}

fn parse_iblank(i: &str) -> IResult<&str, bool> {
    let (i, _) = tag("IBLANK")(i)?;
    let (i, _) = line_ending(i)?;
    let (i, _) = space0(i)?;
    let (i, n) = parse_int(i)?;
    let (i, _) = line_ending(i)?;
    let (i, _) = line_ending(i)?;
    if n == 0 {
        Ok((i, false))
    } else {
        Ok((i, true))
    }
}

fn parse_gvec(i: &str) -> IResult<&str, ()> {
    let (i, _) = tag("GVEC")(i)?;
    let (i, _) = line_ending(i)?;
    let (i, _) = space0(i)?;
    let (i, _) = parse_double(i)?;
    let (i, _) = space0(i)?;
    let (i, _) = parse_double(i)?;
    let (i, _) = space0(i)?;
    let (i, _) = parse_double(i)?;
    let (i, _) = line_ending(i)?;
    let (i, _) = line_ending(i)?;
    Ok((i, ()))
}

fn parse_geom_model(i: &str) -> IResult<&str, String> {
    unimplemented!()
}

#[derive(Clone, Debug)]
pub struct Surface {
    pub name: String,
    a: f64,
    b: f64,
    c: i64,
    d: f64,
    e: f64,
    f: f64,
    g: f64,
    h: f64,
    i: f64,
    j: String,
}

fn parse_surface(i: &str) -> IResult<&str, Surface> {
    let (i, _) = tag("SURFACE")(i)?;
    let (i, _) = line_ending(i)?;
    let (i, _) = space0(i)?;
    let (i, name_full) = not_line_ending(i)?;
    let name = name_full.trim();
    let (i, _) = line_ending(i)?;

    let (i, _) = space0(i)?;
    let (i, a) = parse_double(i)?;
    let (i, _) = space0(i)?;
    let (i, b) = parse_double(i)?;
    let (i, _) = line_ending(i)?;

    let (i, _) = space0(i)?;
    let (i, c) = parse_int(i)?;
    let (i, _) = space0(i)?;
    let (i, d) = parse_double(i)?;
    let (i, _) = space0(i)?;
    let (i, e) = parse_double(i)?;
    let (i, _) = space0(i)?;
    let (i, f) = parse_double(i)?;
    let (i, _) = space0(i)?;
    let (i, g) = parse_double(i)?;
    let (i, _) = space0(i)?;
    let (i, h) = parse_double(i)?;
    let (i, _) = space0(i)?;
    let (i, ix) = parse_double(i)?;
    let (i, _) = space0(i)?;
    let (i, _) = line_ending(i)?;

    let (i, j_full) = not_line_ending(i)?;
    let j = j_full.trim();
    let (i, _) = line_ending(i)?;

    let (i, _) = line_ending(i)?;

    let surf = Surface {
        name: name.to_string(),
        a,
        b,
        c,
        d,
        e,
        f,
        g,
        h,
        i: ix,
        j: j.to_string(),
    };
    Ok((i, surf))
}

#[derive(Clone, Debug)]
pub struct Material {
    pub name: String,
    a: f64,
    b: f64,
    c: f64,
}

fn parse_material(i: &str) -> IResult<&str, Material> {
    let (i, _) = tag("MATERIAL")(i)?;
    let (i, _) = line_ending(i)?;
    let (i, _) = space0(i)?;
    let (i, name_full) = not_line_ending(i)?;
    let name = name_full.trim();
    let (i, _) = line_ending(i)?;

    let (i, _) = space0(i)?;
    let (i, a) = parse_double(i)?;
    let (i, _) = space0(i)?;
    let (i, b) = parse_double(i)?;
    let (i, _) = space0(i)?;
    let (i, c) = parse_double(i)?;
    let (i, _) = space0(i)?;
    let (i, _) = line_ending(i)?;

    let matl = Material {
        name: name.to_string(),
        a,
        b,
        c,
    };
    Ok((i, matl))
}

#[derive(Clone, Debug)]
pub struct PClass {
    pub name: String,
    a: f64,
    b: f64,
    c: f64,
    d: i64,
}
fn parse_pclass(i: &str) -> IResult<&str, PClass> {
    let (i, _) = tag("CLASS_OF_PARTICLES")(i)?;
    let (i, _) = line_ending(i)?;
    let (i, _) = space0(i)?;
    let (i, name_full) = not_line_ending(i)?;
    let name = name_full.trim();
    let (i, _) = line_ending(i)?;

    let (i, _) = space0(i)?;
    let (i, a) = parse_double(i)?;
    let (i, _) = space0(i)?;
    let (i, b) = parse_double(i)?;
    let (i, _) = space0(i)?;
    let (i, c) = parse_double(i)?;
    let (i, _) = space0(i)?;
    let (i, _) = line_ending(i)?;

    let (i, _) = space0(i)?;
    let (i, d) = parse_int(i)?;
    let (i, _) = space0(i)?;
    let (i, _) = line_ending(i)?;

    // TODO: add optional droplet parameter parser
    let (i, _) = line_ending(i)?;

    let pclass = PClass {
        name: name.to_string(),
        a,
        b,
        c,
        d,
    };

    Ok((i, pclass))

    // parseClassOfParticles :: Parser SMVPClass
    // parseClassOfParticles = do
    //     _ <- string "CLASS_OF_PARTICLES" *> eol
    //     c <- onlySpaces *> basicStringSpaces
    //     _ <- onlySpaces
    //     eol
    //     d <- onlySpaces *> floatNum
    //     e <- onlySpaces *> floatNum
    //     f <- onlySpaces *> floatNum
    //     eol
    //     g <- onlySpaces *> intNum
    //     eol
    //     _ <- optionMaybe parseDropletDiameter
    //     eol
    //     return $ SMVPClass c d e f g
}

fn parse_hclass(i: &str) -> IResult<&str, String> {
    unimplemented!()
    // -- TODO: not yet implemented
    // -- TODO: currently overloads the GHC simplifier
    // -- parseClassOfHumans = do
    // --     string "CLASS_OF_HUMANS" *> eol
    // --     name <- onlySpaces *> basicStringSpaces <* onlySpaces <* eol
    // --     d <- onlySpaces *> floatNum
    // --     e <- onlySpaces *> floatNum
    // --     f <- onlySpaces *> floatNum
    // --     eol
    // --     g <- onlySpaces *> intNum
    // --     eol
    // --     onlySpaces
    // --     string "HUMAN_COLOR" *> eol
    // --     colour <- onlySpaces *> basicStringSpaces
    // --     eol
    // --     onlySpaces
    // --     eol
    // --     onlySpaces
    // --     string "HUMAN_FED_DOSE" *> eol
    // --     onlySpaces
    // --     string "FED" *> eol
    // --     onlySpaces
    // --     eol
    // --     onlySpaces
    // --     string "HUMAN_SPEED" *> eol
    // --     onlySpaces
    // --     string "speed" *> eol
    // --     onlySpaces
    // --     string "m/s" *> eol
    // --     -- TODO: unsure of how the below fits, are there one or many
    // --     string "AVATAR_COLOR" *> eol
    // --     onlySpaces
    // --     acN <- intNum
    // --     eol
    // --     let parseLine = do
    // --             c1 <- onlySpaces *> intNum
    // --             c2 <- onlySpaces *> intNum
    // --             c3 <- onlySpaces *> intNum
    // --             eol
    // --     cs <- count acN parseLine
    // --     eol
    // --     return () -- (SMVPClass name d e f g)
}

fn parse_droplet_diameter(i: &str) -> IResult<&str, String> {
    unimplemented!()
    // parseDropletDiameter :: Parser ()
    // parseDropletDiameter = do
    //     _ <- onlySpaces
    //     _ <- string "DROPLET DIAMETER"
    //     _ <- onlySpaces
    //     eol
    //     _ <- onlySpaces
    //     _ <- string "diam"
    //     _ <- onlySpaces
    //     eol
    //     _ <- onlySpaces
    //     _ <- string "mu-m"
    //     _ <- onlySpaces
    //     eol
}

fn parse_outline(i: &str) -> IResult<&str, Vec<OutlineEntry>> {
    let (i, _) = tag("OUTLINE")(i)?;
    let (i, _) = line_ending(i)?;

    let (i, _) = space0(i)?;
    let (i, a) = parse_int(i)?;
    let (i, _) = space0(i)?;
    let (i, _) = line_ending(i)?;

    let (i, entries) = many0(parse_outline_entry)(i)?;

    let (i, _) = line_ending(i)?;
    Ok((i, entries))
}

#[derive(Clone, Debug)]
pub struct OutlineEntry {
    pub a: f64,
    pub b: f64,
    pub c: f64,
    pub d: f64,
    pub e: f64,
    pub f: f64,
}

fn parse_outline_entry(i: &str) -> IResult<&str, OutlineEntry> {
    let (i, _) = space0(i)?;
    let (i, a) = parse_double(i)?;
    let (i, _) = space0(i)?;
    let (i, b) = parse_double(i)?;
    let (i, _) = space0(i)?;
    let (i, c) = parse_double(i)?;
    let (i, _) = space0(i)?;
    let (i, d) = parse_double(i)?;
    let (i, _) = space0(i)?;
    let (i, e) = parse_double(i)?;
    let (i, _) = space0(i)?;
    let (i, f) = parse_double(i)?;
    let (i, _) = space0(i)?;
    let (i, _) = line_ending(i)?;

    let outline_entry = OutlineEntry { a, b, c, d, e, f };

    Ok((i, outline_entry))
}

#[derive(Clone, Debug)]
pub struct Prop {
    pub name: String,
    pub type_: String,
    a: i64,
    b: i64,
}
fn parse_prop(i: &str) -> IResult<&str, Prop> {
    let (i, _) = tag("PROP")(i)?;
    let (i, _) = line_ending(i)?;

    let (i, _) = space0(i)?;
    let (i, name_full) = not_line_ending(i)?;
    let name = name_full.trim();
    let (i, _) = line_ending(i)?;

    let (i, _) = space0(i)?;
    let (i, a) = parse_int(i)?;
    let (i, _) = line_ending(i)?;

    let (i, _) = space0(i)?;
    let (i, type_full) = not_line_ending(i)?;
    let type_ = type_full.trim();
    let (i, _) = line_ending(i)?;

    let (i, _) = space0(i)?;
    let (i, b) = parse_int(i)?;
    let (i, _) = line_ending(i)?;

    let (i, _) = line_ending(i)?;

    let prop = Prop {
        name: name.to_string(),
        type_: type_.to_string(),
        a,
        b,
    };

    Ok((i, prop))
}

#[derive(Clone, Debug)]
pub struct Device {
    pub name: String,
    pub prop: String,
    pub quantity: String,
    a: f64,
    b: f64,
    c: f64,
    d: f64,
    e: f64,
    f: f64,
    g: i64,
    h: i64,
}
fn parse_device(i: &str) -> IResult<&str, Device> {
    let (i, _) = tag("DEVICE")(i)?;
    let (i, _) = line_ending(i)?;

    let (i, _) = space0(i)?;
    // TODO: just parsing until % is a big problem
    let (i, name_full) = many0(none_of("%"))(i)?;
    let name_s = name_full.into_iter().collect::<String>();
    let name = name_s.trim();
    let (i, _) = space0(i)?;
    let (i, _) = char('%')(i)?;
    let (i, _) = space0(i)?;
    let (i, quantity_full) = not_line_ending(i)?;
    let quantity = quantity_full.trim();
    let (i, _) = line_ending(i)?;

    let (i, _) = space0(i)?;
    let (i, a) = parse_double(i)?;
    let (i, _) = space0(i)?;
    let (i, b) = parse_double(i)?;
    let (i, _) = space0(i)?;
    let (i, c) = parse_double(i)?;
    let (i, _) = space0(i)?;
    let (i, d) = parse_double(i)?;
    let (i, _) = space0(i)?;
    let (i, e) = parse_double(i)?;
    let (i, _) = space0(i)?;
    let (i, f) = parse_double(i)?;
    let (i, _) = space0(i)?;
    let (i, g) = parse_int(i)?;
    let (i, _) = space0(i)?;
    let (i, h) = parse_int(i)?;
    let (i, _) = space0(i)?;
    let (i, _) = char('%')(i)?;
    let (i, _) = char(' ')(i)?;
    let (i, prop_full) = not_line_ending(i)?;
    let prop = prop_full.trim();
    let (i, _) = line_ending(i)?;

    let devc = Device {
        name: name.to_string(),
        prop: prop.to_string(),
        quantity: quantity.to_string(),
        a,
        b,
        c,
        d,
        e,
        f,
        g,
        h,
    };

    let (i, _) = line_ending(i)?;

    Ok((i, devc))
}

#[derive(Clone, Debug)]
pub struct Mesh {
    pub name: String,
    pub offset: (f64, f64, f64),
    pub grid: ((i64, i64, i64), i64),
    pub bbox: (f64, f64, f64, f64, f64, f64),
    pub colour: (f64, f64, f64),
    pub trns: MeshTRNs,
    pub obsts: Vec<Obst>,
    pub vents: Vec<Vent>,
}
fn parse_mesh(i: &str) -> IResult<&str, Mesh> {
    let (i, _) = tag("OFFSET")(i)?;
    let (i, _) = line_ending(i)?;
    let (i, _) = space0(i)?;
    let (i, offset1) = parse_double(i)?;
    let (i, _) = space0(i)?;
    let (i, offset2) = parse_double(i)?;
    let (i, _) = space0(i)?;
    let (i, offset3) = parse_double(i)?;
    let (i, _) = space0(i)?;
    let (i, _) = line_ending(i)?;
    let (i, _) = line_ending(i)?;

    let (i, _) = tag("GRID")(i)?;
    let (i, _) = space0(i)?;
    let (i, name_full) = not_line_ending(i)?;
    let name = name_full.trim();
    let (i, _) = line_ending(i)?;
    let (i, _) = space0(i)?;
    let (i, n_x_cells) = parse_int(i)?;
    let (i, _) = space0(i)?;
    let (i, n_y_cells) = parse_int(i)?;
    let (i, _) = space0(i)?;
    let (i, n_z_cells) = parse_int(i)?;
    let (i, _) = space0(i)?;
    let (i, grid_4_unknown) = parse_int(i)?;
    let (i, _) = space0(i)?;
    let (i, _) = line_ending(i)?;
    let (i, _) = line_ending(i)?;

    let (i, pdim) = parse_pdims(i)?;
    let (i, trns) = parse_trns(i)?;
    let (i, obsts) = parse_obsts(i)?;
    let (i, vents) = parse_vents(i)?;
    let (i, cvents) = parse_cvents(i)?;

    let mesh = Mesh {
        name: name.to_string(),
        offset: (offset1, offset2, offset3),
        grid: ((n_x_cells, n_y_cells, n_z_cells), grid_4_unknown),
        bbox: pdim.xb,
        colour: pdim.rgb,
        trns,
        obsts,
        vents,
    };
    let (i, _) = opt(line_ending)(i)?;
    Ok((i, mesh))
}

#[derive(Clone, Debug)]
pub struct PDim {
    pub xb: (f64, f64, f64, f64, f64, f64),
    pub rgb: (f64, f64, f64),
}

fn parse_pdims(i: &str) -> IResult<&str, PDim> {
    let (i, _) = tag("PDIM")(i)?;
    let (i, _) = line_ending(i)?;

    let (i, _) = space0(i)?;
    let (i, x_min) = parse_double(i)?;
    let (i, _) = space0(i)?;
    let (i, x_max) = parse_double(i)?;
    let (i, _) = space0(i)?;
    let (i, y_min) = parse_double(i)?;
    let (i, _) = space0(i)?;
    let (i, y_max) = parse_double(i)?;
    let (i, _) = space0(i)?;
    let (i, z_min) = parse_double(i)?;
    let (i, _) = space0(i)?;
    let (i, z_max) = parse_double(i)?;
    let (i, _) = space0(i)?;
    let (i, r) = parse_double(i)?;
    let (i, _) = space0(i)?;
    let (i, g) = parse_double(i)?;
    let (i, _) = space0(i)?;
    let (i, b) = parse_double(i)?;
    let (i, _) = space0(i)?;
    let (i, _) = line_ending(i)?;

    let (i, _) = line_ending(i)?;

    let pdim = PDim {
        xb: (x_min, x_max, y_min, y_max, z_min, z_max),
        rgb: (r, g, b),
    };
    Ok((i, pdim))
}
#[derive(Clone, Debug)]
pub struct MeshTRNs {
    pub trnx: Vec<(i64, f64)>,
    pub trny: Vec<(i64, f64)>,
    pub trnz: Vec<(i64, f64)>,
}
// getMeshBounds :: MeshTRNs -> (Double, Double, Double, Double, Double, Double)
// getMeshBounds meshTRNs@(MeshTRNs trnx trny trnz) = (xMin, xMax, yMin, yMax, zMin, zMax)
//     where
//         (_, xMin) = headErr "trnx" trnx
//         (_, xMax) = last trnx
//         (_, yMin) = headErr "trny" trny
//         (_, yMax) = last trny
//         (_, zMin) = headErr "trnx" trnz
//         (_, zMax) = last trnz
// -- TODO: this uses ijk at the min corner, not cell centre. Check for appropriateness.
// getCellXB
//     :: MeshTRNs -- ^TRNs for the particular mesh
//     -> (Int, Int, Int)  -- ^The ijk value for the cell (from zero)
//     -> Either String (Double, Double, Double, Double, Double, Double)
// getCellXB trns@(MeshTRNs trnx trny trnz) cell@(i, j, k) = do
//     xs <- trnLookup trnx i
//     ys <- trnLookup trny j
//     zs <- trnLookup trnz k
//     return $ rearrange xs ys zs
//     where   -- TODO: fix inconsistent error handling.
//         rearrange (x1, x2) (y1, y2) (z1, z2) = (x1, x2, y1, y2, z1, z2)
//         trnLookup :: [(Int,Double)] -> Int -> Either String (Double, Double)
//         trnLookup (t:[]) n =
//             Left $ "getCellXB: TRN lookup failed for "
//                 ++ show n ++ ". Max TRN " ++ show t ++ "."
//         trnLookup (t1@(t1N,t1V):t2@(t2N,t2V):trn) n
//             = if t1N == n
//                 then Right (t1V,t2V)
//                 else trnLookup (t2:trn) n

// getCornerXYZ :: MeshTRNs -> (Int, Int, Int) -> Either String (Double, Double, Double)
// getCornerXYZ trns@(MeshTRNs trnx trny trnz) corner@(i, j, k) = do
//     x <- case lookup i trnx of
//                 Just x -> Right x
//                 Nothing -> Left $ "getCornerXYZ: TRNx lookup failed for " ++ show i
//     y <- case lookup j trny of
//                 Just y -> Right y
//                 Nothing -> Left $ "getCornerXYZ: TRNy lookup failed for " ++ show j
//     z <- case lookup k trnz of
//                 Just z -> Right z
//                 Nothing -> Left $ "getCornerXYZ: TRNz lookup failed for " ++ show k
//     return (x, y, z)

// -- |Get the minimum corner IJK value of the cell which contains a point
// xyzToIJKMinCorner :: MeshTRNs -> (Double, Double, Double) -> Either String (Int, Int, Int)
// xyzToIJKMinCorner trns@(MeshTRNs trnx trny trnz) xyz@(x,y,z) = do
//     i <- case ijkLookupSingle x trnx of
//             Left e -> Left $ e ++ show trnx
//             Right x -> Right x
//     j <- case ijkLookupSingle y trny of
//             Left e -> Left $ e ++ show trny
//             Right y -> Right y
//     k <- case ijkLookupSingle z trnz of
//             Left e -> Left $ e ++ show trnz
//             Right z -> Right z
//     return (i, j, k)
//     where
//         epsilon = 0.001  -- TODO: investigate the necessity of this error allowance.
//         ijkLookupSingle :: Double -> [(Int,Double)] -> Either String Int
//         ijkLookupSingle x ((tN,tV):trns) =
//             if (x+epsilon) < tV  -- TODO: investigate this error allowance
//                 then error $ show (x+epsilon) ++ " is below the minimum trn value of " ++ show tV
//                 else ijkLookupSingleWorker x ((tN,tV):trns)
//         ijkLookupSingleWorker :: Double -> [(Int,Double)] -> Either String Int
//         ijkLookupSingleWorker x ((tN,tV):(tN2,tV2):[]) =
//             if (x-epsilon) <= tV2 -- TODO: investigate this error allowance
//                 then Right tN
//                 else Left $ "xyzToIJK: TRN lookup failed: " ++ show (x-epsilon) ++ " is not <= to " ++ show tV2 ++ "."
//         ijkLookupSingleWorker x ((tN,tV):(tN2,tV2):trn)
//             = if x <= tV2 then Right tN else ijkLookupSingle x ((tN2,tV2):trn)

// getCellCentre :: MeshTRNs -> (Int, Int, Int) -> Either String (Double, Double, Double)
// getCellCentre trns cellIJK = case getCellXB trns cellIJK of
//                 Right (x1, x2, y1, y2, z1, z2) -> Right ((x1+x2)/2, (y1+y2)/2, (z1+z2)/2)
//                 Left x -> Left x

// getCellLocation :: MeshTRNs -> (Int, Int, Int) -> Either String (Double, Double, Double)
// getCellLocation trns cellIJK = case getCellXB trns cellIJK of
//                 Right (x1, x2, y1, y2, z1, z2) -> Right (x1, y1, z1)
//                 Left x -> Left x

// -- getCellAxisLocation trns@(MeshTRNs trnx trny trnz) cell@(i, j, k) = (x, y, z)
fn parse_trns(i: &str) -> IResult<&str, MeshTRNs> {
    let (i, _) = tag("TRNX")(i)?;
    let (i, _) = line_ending(i)?;
    let (i, trnx) = parse_trn(i)?;
    let (i, _) = tag("TRNY")(i)?;
    let (i, _) = line_ending(i)?;
    let (i, trny) = parse_trn(i)?;
    let (i, _) = tag("TRNZ")(i)?;
    let (i, _) = line_ending(i)?;
    let (i, trnz) = parse_trn(i)?;
    let trns = MeshTRNs { trnx, trny, trnz };
    Ok((i, trns))
}

fn parse_trn(i: &str) -> IResult<&str, Vec<(i64, f64)>> {
    let (i, _) = space0(i)?;
    let (i, n) = parse_int(i)?;
    let (i, _) = line_ending(i)?;
    let (i, entries) = many0(parse_trn_entry)(i)?;
    let (i, _) = line_ending(i)?;
    Ok((i, entries))
}

fn parse_trn_entry(i: &str) -> IResult<&str, (i64, f64)> {
    let (i, _) = space0(i)?;
    let (i, n) = parse_int(i)?;
    let (i, _) = space0(i)?;
    let (i, f) = parse_double(i)?;
    let (i, _) = space0(i)?;
    let (i, _) = line_ending(i)?;
    Ok((i, (n, f)))
}

#[derive(Clone, Debug)]
pub struct Obst {
    pub xb: (f64, f64, f64, f64, f64, f64),
    pub id: i64,
    pub surfaces: (u64, u64, u64, u64, u64, u64),
    pub ijk: (i64, i64, i64, i64, i64, i64),
    pub colour_index: i64,
    pub block_type: i64,
}
fn parse_obsts(i: &str) -> IResult<&str, Vec<Obst>> {
    let (i, _) = tag("OBST")(i)?;
    let (i, _) = line_ending(i)?;
    let (i, _) = space0(i)?;
    let (i, n) = parse_int(i)?;
    let (i, _) = space0(i)?;
    let (i, _) = line_ending(i)?;
    let (i, line1s) = nom::multi::count(parse_obst_line1, n as usize)(i)?;
    let (i, line2s) = nom::multi::count(parse_obst_line2, n as usize)(i)?;
    let (i, _) = line_ending(i)?;
    let mut obsts = Vec::with_capacity(n as usize);
    for (a, b) in line1s.into_iter().zip(line2s.into_iter()) {
        obsts.push(stitch_obst(a, b));
    }
    Ok((i, obsts))
}
fn stitch_obst(
    l1: (
        (f64, f64, f64, f64, f64, f64),
        i64,
        (u64, u64, u64, u64, u64, u64),
    ),
    l2: ((i64, i64, i64, i64, i64, i64), i64, i64),
) -> Obst {
    Obst {
        xb: l1.0,
        id: l1.1,
        surfaces: l1.2,
        ijk: l2.0,
        colour_index: l2.1,
        block_type: l2.2,
    }
}

#[derive(Clone, Debug)]
pub struct Vent {
    pub xb: (f64, f64, f64, f64, f64, f64),
    pub unknown1: i64,
    pub unknown2: i64,
    pub unknown3: i64,
    pub unknown4: i64,
    pub unknown5: i64,
    pub unknown6: i64,
    pub unknown7: i64,
    pub unknown8: i64,
    pub unknown9: i64,
    pub unknown10: i64,
}
fn parse_vents(i: &str) -> IResult<&str, Vec<Vent>> {
    let (i, _) = tag("VENT")(i)?;
    let (i, _) = line_ending(i)?;
    let (i, _) = space0(i)?;
    let (i, n) = parse_int(i)?;
    let (i, _) = space0(i)?;
    let (i, n_dummy) = parse_int(i)?;
    let (i, _) = space0(i)?;
    let (i, _) = line_ending(i)?;
    let (i, line1s) = nom::multi::count(parse_vent_line1, n as usize)(i)?;
    let (i, line2s) = nom::multi::count(parse_vent_line2, n as usize)(i)?;
    let (i, _) = line_ending(i)?;
    let mut vents = Vec::with_capacity(n as usize);
    for (a, b) in line1s.into_iter().zip(line2s.into_iter()) {
        vents.push(stitch_vent(a, b));
    }
    Ok((i, vents))
}

fn stitch_vent(
    l1: ((f64, f64, f64, f64, f64, f64), i64, i64),
    l2: (i64, i64, i64, i64, i64, i64, i64, i64),
) -> Vent {
    Vent {
        xb: l1.0,
        unknown1: l1.1,
        unknown2: l1.2,
        unknown3: l2.0,
        unknown4: l2.1,
        unknown5: l2.2,
        unknown6: l2.3,
        unknown7: l2.4,
        unknown8: l2.5,
        unknown9: l2.6,
        unknown10: l2.7,
    }
}

#[derive(Clone, Debug)]
pub struct CVent {}

fn parse_cvents(i: &str) -> IResult<&str, Vec<CVent>> {
    let (i, _) = tag("CVENT")(i)?;
    let (i, _) = line_ending(i)?;
    let (i, _) = space0(i)?;
    let (i, n) = parse_int(i)?;
    let (i, _) = space0(i)?;
    let (i, _) = line_ending(i)?;
    let (i, cvents) = nom::multi::count(parse_cvent, n as usize)(i)?;
    let (i, _) = line_ending(i)?;
    Ok((i, cvents))
}

fn parse_cvent(i: &str) -> IResult<&str, CVent> {
    unimplemented!()
}

fn parse_obst_line1(
    i: &str,
) -> IResult<
    &str,
    (
        (f64, f64, f64, f64, f64, f64),
        i64,
        (u64, u64, u64, u64, u64, u64),
    ),
> {
    let (i, _) = space0(i)?;
    let (i, x1) = parse_double(i)?;
    let (i, _) = space0(i)?;
    let (i, x2) = parse_double(i)?;
    let (i, _) = space0(i)?;
    let (i, y1) = parse_double(i)?;
    let (i, _) = space0(i)?;
    let (i, y2) = parse_double(i)?;
    let (i, _) = space0(i)?;
    let (i, z1) = parse_double(i)?;
    let (i, _) = space0(i)?;
    let (i, z2) = parse_double(i)?;
    let (i, _) = space0(i)?;
    let (i, id) = parse_int(i)?;
    let (i, _) = space0(i)?;
    let (i, s1) = parse_uint(i)?;
    let (i, _) = space0(i)?;
    let (i, s2) = parse_uint(i)?;
    let (i, _) = space0(i)?;
    let (i, s3) = parse_uint(i)?;
    let (i, _) = space0(i)?;
    let (i, s4) = parse_uint(i)?;
    let (i, _) = space0(i)?;
    let (i, s5) = parse_uint(i)?;
    let (i, _) = space0(i)?;
    let (i, s6) = parse_uint(i)?;
    let (i, _) = space0(i)?;
    let (i, extra) = opt(parse_obst1_extra)(i)?;
    let (i, _) = line_ending(i)?;
    Ok((i, ((x1, x2, y1, y2, z1, z2), id, (s1, s2, s3, s4, s5, s6))))
}

fn parse_obst1_extra(i: &str) -> IResult<&str, (f64, f64, f64)> {
    let (i, _) = space0(i)?;
    let (i, op_a) = parse_double(i)?;
    let (i, _) = space0(i)?;
    let (i, op_b) = parse_double(i)?;
    let (i, _) = space0(i)?;
    let (i, op_c) = parse_double(i)?;
    let (i, _) = space0(i)?;
    Ok((i, (op_a, op_b, op_c)))
}

fn parse_obst_line2(i: &str) -> IResult<&str, ((i64, i64, i64, i64, i64, i64), i64, i64)> {
    let (i, _) = space0(i)?;
    let (i, i1) = parse_int(i)?;
    let (i, _) = space0(i)?;
    let (i, i2) = parse_int(i)?;
    let (i, _) = space0(i)?;
    let (i, j1) = parse_int(i)?;
    let (i, _) = space0(i)?;
    let (i, j2) = parse_int(i)?;
    let (i, _) = space0(i)?;
    let (i, k1) = parse_int(i)?;
    let (i, _) = space0(i)?;
    let (i, k2) = parse_int(i)?;
    let (i, _) = space0(i)?;
    let (i, colorindex) = parse_int(i)?;
    let (i, _) = space0(i)?;
    let (i, blocktype) = parse_int(i)?;
    let (i, _) = space0(i)?;
    let (i, extra) = opt(parse_obst2_extra)(i)?;
    let (i, _) = line_ending(i)?;
    Ok((i, ((i1, i2, j1, j2, k1, k2), colorindex, blocktype)))
}

fn parse_obst2_extra(i: &str) -> IResult<&str, (f64, f64, f64, f64)> {
    let (i, _) = space0(i)?;
    let (i, op_a) = parse_double(i)?;
    let (i, _) = space0(i)?;
    let (i, op_b) = parse_double(i)?;
    let (i, _) = space0(i)?;
    let (i, op_c) = parse_double(i)?;
    let (i, _) = space0(i)?;
    let (i, op_d) = parse_double(i)?;
    let (i, _) = space0(i)?;
    Ok((i, (op_a, op_b, op_c, op_d)))
}

fn parse_vent_line1(i: &str) -> IResult<&str, ((f64, f64, f64, f64, f64, f64), i64, i64)> {
    let (i, _) = space0(i)?;
    let (i, x1) = parse_double(i)?;
    let (i, _) = space0(i)?;
    let (i, x2) = parse_double(i)?;
    let (i, _) = space0(i)?;
    let (i, y1) = parse_double(i)?;
    let (i, _) = space0(i)?;
    let (i, y2) = parse_double(i)?;
    let (i, _) = space0(i)?;
    let (i, z1) = parse_double(i)?;
    let (i, _) = space0(i)?;
    let (i, z2) = parse_double(i)?;
    let (i, _) = space0(i)?;
    let (i, g) = parse_int(i)?;
    let (i, _) = space0(i)?;
    let (i, h) = parse_int(i)?;
    let (i, _) = space0(i)?;
    let (i, extra) = opt(parse_vent1_extra)(i)?;
    let (i, _) = line_ending(i)?;
    Ok((i, ((x1, x2, y1, y2, z1, z2), g, h)))
}

fn parse_vent1_extra(i: &str) -> IResult<&str, (f64, f64, f64)> {
    let (i, _) = space0(i)?;
    let (i, op_a) = parse_double(i)?;
    let (i, _) = space0(i)?;
    let (i, op_b) = parse_double(i)?;
    let (i, _) = space0(i)?;
    let (i, op_c) = parse_double(i)?;
    let (i, _) = space0(i)?;
    Ok((i, (op_a, op_b, op_c)))
}
// parseVentLine1 = do
//     x1 <- onlySpaces *> floatNum
//     x2 <- onlySpaces *> floatNum
//     y1 <- onlySpaces *> floatNum
//     y2 <- onlySpaces *> floatNum
//     z1 <- onlySpaces *> floatNum
//     z2 <- onlySpaces *> floatNum
//     g <- onlySpaces *> intNum
//     h <- onlySpaces *> intNum
//     optionMaybe (onlySpaces *> floatNum *> onlySpaces *> floatNum *> onlySpaces *> floatNum *> pure ())
//     eol

//     return ((x1, x2, y1, y2, z1, z2), g, h)

fn parse_vent_line2(i: &str) -> IResult<&str, (i64, i64, i64, i64, i64, i64, i64, i64)> {
    let (i, _) = space0(i)?;
    let (i, a) = parse_int(i)?;
    let (i, _) = space0(i)?;
    let (i, b) = parse_int(i)?;
    let (i, _) = space0(i)?;
    let (i, c) = parse_int(i)?;
    let (i, _) = space0(i)?;
    let (i, d) = parse_int(i)?;
    let (i, _) = space0(i)?;
    let (i, e) = parse_int(i)?;
    let (i, _) = space0(i)?;
    let (i, f) = parse_int(i)?;
    let (i, _) = space0(i)?;
    let (i, g) = parse_int(i)?;
    let (i, _) = space0(i)?;
    let (i, h) = parse_int(i)?;
    let (i, _) = space0(i)?;
    let (i, extra) = opt(parse_vent2_extra)(i)?;
    let (i, _) = line_ending(i)?;
    Ok((i, (a, b, c, d, e, f, g, h)))
}

fn parse_vent2_extra(i: &str) -> IResult<&str, (f64, f64, f64, f64)> {
    let (i, _) = space0(i)?;
    let (i, op_a) = parse_double(i)?;
    let (i, _) = space0(i)?;
    let (i, op_b) = parse_double(i)?;
    let (i, _) = space0(i)?;
    let (i, op_c) = parse_double(i)?;
    let (i, _) = space0(i)?;
    let (i, op_d) = parse_double(i)?;
    let (i, _) = space0(i)?;
    Ok((i, (op_a, op_b, op_c, op_d)))
}
// parseAlias :: Parser String
// parseAlias =  char '%' *> manyTill anyChar (try (string " &"))
fn parse_face(i: &str) -> IResult<&str, String> {
    unimplemented!()
}

fn parse_vert(i: &str) -> IResult<&str, String> {
    unimplemented!()
}

fn parse_tail_entry(i: &str) -> IResult<&str, TailEntry> {
    let (i, entry_name) = nom::character::complete::alphanumeric1(i)?;

    match entry_name {
        "SLCF" => unimplemented!(),
        //                     meshNum <- onlySpaces *> intNum
        //                     alias <- onlySpaces *> optionMaybe parseAlias
        //                     clearLine   -- TODO: change
        //                     -- eol
        //                     filename <- onlySpaces *> fullString <* eol
        //                     longName <- onlySpaces *> basicStringSpacesExtra <* eol
        //                     shortName <- onlySpaces *> basicStringSpacesExtra <* eol
        //                     units <- onlySpaces *> basicStringExtra
        //                     -- units' <- optionMaybe basicStringExtra
        //                     -- let units = case units' of
        //                             -- Just x -> x
        //                             -- Nothing -> "unknown"
        //                     eol
        //                     return $ DataFileTailEntry $ SLCFDataFile meshNum filename longName shortName units alias
        "SLCC" => unimplemented!(),
        //                     meshNum <- onlySpaces *> intNum
        //                     alias <- onlySpaces *> optionMaybe parseAlias
        //                     clearLine   -- TODO: change
        //                     -- eol
        //                     filename <- onlySpaces *> fullString <* eol
        //                     longName <- onlySpaces *> basicStringSpacesExtra <* eol
        //                     shortName <- onlySpaces *> basicStringSpacesExtra <* eol
        //                     units <- onlySpaces *> basicStringExtra
        //                     -- units' <- optionMaybe basicStringExtra
        //                     -- let units = case units' of
        //                             -- Just x -> x
        //                             -- Nothing -> "unknown"
        //                     eol
        //                     return $ DataFileTailEntry $ SLCFDataFile
        //                       { slcfMeshNum = meshNum
        //                       , slcfFilename = filename
        //                       , slcfLongName = longName
        //                       , slcfShortName = shortName
        //                       , slcfUnits = units
        //                       , slcfAlias = alias
        //                       }
        "BNDF" => unimplemented!(),
        //                     meshNum <- onlySpaces *> intNum
        //                     anotherNum <- onlySpaces *> intNum <* eol   -- TODO: find out what this number is and include in datatype
        //                     filename <- onlySpaces *> fullString <* eol
        //                     longName <- onlySpaces *> basicStringSpacesExtra <* eol
        //                     shortName <- onlySpaces *> basicStringSpacesExtra <* eol
        //                     units <- onlySpaces *> basicStringExtra
        //                     -- units' <- optionMaybe basicStringExtra
        //                     -- let units = case units' of
        //                             -- Just x -> x
        //                             -- Nothing -> "unknown"
        //                     eol
        //                     return $ DataFileTailEntry $ BNDFDataFile meshNum filename longName shortName units
        "BNDC" => unimplemented!(), // TODO: BNDF and BNDC are the same
        //                     meshNum <- onlySpaces *> intNum
        //                     anotherNum <- onlySpaces *> intNum <* eol   -- TODO: find out what this number is and include in datatype
        //                     filename <- onlySpaces *> fullString <* eol
        //                     longName <- onlySpaces *> basicStringSpacesExtra <* eol
        //                     shortName <- onlySpaces *> basicStringSpacesExtra <* eol
        //                     units <- onlySpaces *> basicStringExtra
        //                     -- units' <- optionMaybe basicStringExtra
        //                     -- let units = case units' of
        //                             -- Just x -> x
        //                             -- Nothing -> "unknown"
        //                     eol
        //                     return $ DataFileTailEntry $ BNDFDataFile meshNum filename longName shortName units
        "PRT5" => unimplemented!(),
        //                     meshNum <- onlySpaces *> intNum <* eol
        //                     filename <- onlySpaces *> fullString <* eol
        //                     (aNum:bNum:_) <- many1 $ do
        //                         onlySpaces
        //                         aNum <- intNum
        //                         eol
        //                         return aNum
        //                     return $ DataFileTailEntry $ PRT5DataFile meshNum filename aNum bNum
        "SMOKE3D" => unimplemented!(),
        //                     meshNum <- onlySpaces *> intNum <* eol
        //                     filename <- onlySpaces *> fullString <* eol
        //                     longName <- onlySpaces *> basicStringSpacesExtra <* eol
        //                     shortName <- onlySpaces *> basicStringExtra <* eol
        //                     units <- onlySpaces *> basicStringExtra <* eol
        //                     return $ DataFileTailEntry $ SMOKE3DDataFile meshNum filename longName shortName units
        "SMOKF3D" => {
            let (i, _) = space0(i)?;
            let (i, mesh_num) = parse_int(i)?;
            let (i, _) = line_ending(i)?;
            let (i, _) = space0(i)?;
            let (i, filename_full) = not_line_ending(i)?;
            let filename = filename_full.trim();
            let (i, _) = line_ending(i)?;
            let (i, _) = space0(i)?;
            let (i, longname_full) = not_line_ending(i)?;
            let longname = longname_full.trim();
            let (i, _) = line_ending(i)?;
            let (i, _) = space0(i)?;
            let (i, shortname_full) = not_line_ending(i)?;
            let shortname = shortname_full.trim();
            let (i, _) = line_ending(i)?;
            let (i, _) = space0(i)?;
            let (i, units_full) = not_line_ending(i)?;
            let units = units_full.trim();
            let (i, _) = line_ending(i)?;
            Ok((
                i,
                TailEntry::DataFile(DataFileEntry::Smoke3d(Smoke3dDataFile {
                    mesh_num: mesh_num,
                    filename: filename.to_string(),
                    longname: longname.to_string(),
                    shortname: shortname.to_string(),
                    units: units.to_string(),
                })),
            ))
        }
        //                     meshNum <- onlySpaces *> intNum <* eol
        //                     filename <- onlySpaces *> fullString <* eol
        //                     longName <- onlySpaces *> basicStringSpacesExtra <* eol
        //                     shortName <- onlySpaces *> basicStringExtra <* eol
        //                     units <- onlySpaces *> basicStringExtra <* eol
        //                     return $ DataFileTailEntry $ SMOKE3DDataFile meshNum filename longName shortName units
        "PL3D" => unimplemented!(),
        //                     time <- onlySpaces *> floatNum
        //                     meshNum <- onlySpaces *> intNum <* eol
        //                     filename <- onlySpaces *> fullString <* eol
        //                     pl3dEntries <- many parsePL3DEntry
        //                     return $ DataFileTailEntry $ PL3DDataFile time meshNum filename pl3dEntries
        "XYZ" => {
            let (i, _) = line_ending(i)?;
            let (i, name_full) = not_line_ending(i)?;
            let name = name_full.trim();
            let (i, _) = line_ending(i)?;
            Ok((
                i,
                TailEntry::DataFile(DataFileEntry::XYZ(XYZDataFile {
                    filename: name.to_string(),
                })),
            ))
        }
        "ISOG" => unimplemented!(),
        //                     meshNum <- onlySpaces *> intNum <* eol
        //                     filename <- onlySpaces *> fullString <* eol
        //                     longName <- onlySpaces *> basicStringSpacesExtra <* eol
        //                     shortName <- onlySpaces *> basicStringExtra <* eol
        //                     units <- onlySpaces *> basicStringExtra <* eol
        //                     return $ DataFileTailEntry $ SMOKE3DDataFile meshNum filename longName shortName units
        "DEVICE_ACT" => unimplemented!(),
        //                     devcName <- onlySpaces *> deviceNameParser <* eol <* onlySpaces
        //                     d1 <- intNum <* onlySpaces
        //                     d2 <- floatNum <* onlySpaces
        //                     d3 <- intNum <* eol
        //                     return $ DeviceActivationTailEntry $ DevcActEntry devcName d1 d2 d3
        "SHOW_OBST" => unimplemented!(),
        //                     k1 <- onlySpaces *> intNum <* eol
        //                     k2 <- onlySpaces *> intNum
        //                     k3 <- onlySpaces *> floatNum <* eol
        //                     return $ ObstVisTailEntry $ ShowObst k1 k2 k3
        "HIDE_OBST" => unimplemented!(),
        //                     j1 <- onlySpaces *> intNum <* eol
        //                     j2 <- onlySpaces *> intNum
        //                     j3 <- onlySpaces *> floatNum <* eol
        //                     return $ ObstVisTailEntry $ HideObst j1 j2 j3
        "CLOSE_VENT" => unimplemented!(),
        //                     j1 <- onlySpaces *> intNum <* eol
        //                     j2 <- onlySpaces *> intNum
        //                     j3 <- onlySpaces *> floatNum <* eol
        //                     return $ ObstVisTailEntry $ HideObst j1 j2 j3   --TODO: incorrect entry
        "OPEN_VENT" => unimplemented!(),
        //                     j1 <- onlySpaces *> intNum <* eol
        //                     j2 <- onlySpaces *> intNum
        //                     j3 <- onlySpaces *> floatNum <* eol
        //                     return $ ObstVisTailEntry $ HideObst j1 j2 j3 --TODO: incorrect entry
        _ => panic!(
            "The following was parsed by \"parseDataFile\" >{}<",
            entry_name
        ),
    }
}

fn sort_tail_entries(
    tail_entries: Vec<TailEntry>,
) -> (Vec<DataFileEntry>, Vec<DevcActEntry>, Vec<ObstVisEntry>) {
    unimplemented!()
    // sortTailEntries :: [TailEntry] -> ([DataFileEntry], [DevcActEntry], [ObstVisEntry])
    // sortTailEntries tailEntries = foldl' sortEntry ([], [], []) tailEntries
    //     where
    //         sortEntry (dataFileEntries, devActEntries, obstVisEntries) entry =
    //             case entry of
    //                 DataFileTailEntry x -> (x:dataFileEntries, devActEntries, obstVisEntries)
    //                 DeviceActivationTailEntry x -> (dataFileEntries, x:devActEntries, obstVisEntries)
    //                 ObstVisTailEntry x -> (dataFileEntries, devActEntries, x:obstVisEntries)
}

#[derive(Clone, Debug)]
pub enum TailEntry {
    DataFile(DataFileEntry),
    DeviceActivation(DevcActEntry),
    ObstVisTail(ObstVisEntry),
}

#[derive(Clone, Debug)]
pub enum DataFileEntry {
    XYZ(XYZDataFile),
    Smoke3d(Smoke3dDataFile),
}

#[derive(Clone, Debug)]
pub struct XYZDataFile {
    pub filename: String,
}
#[derive(Clone, Debug)]
pub struct Smoke3dDataFile {
    pub mesh_num: i64,
    pub filename: String,
    pub longname: String,
    pub shortname: String,
    pub units: String,
}
// data DataFileEntry   -- TODO: Considere moving each datafile type to its own datatype and making DataFileEntry a Typeclass
//     = SLCFDataFile
//         { slcfMeshNum   :: Int
//         , slcfFilename  :: String
//         , slcfLongName  :: String
//         , slcfShortName :: String
//         , slcfUnits     :: String
//         , slcfAlias     :: Maybe String
//         }-- n filename longName shortName units
//     | BNDFDataFile
//         { bndfMeshNum   :: Int
//         , bndfFilename  :: String
//         , bndfLongName  :: String
//         , bndfShortName :: String
//         , bndfUnits     :: String
//         }-- n filename longName shortName units
//     | PRT5DataFile
//         { prt5MeshNum   :: Int
//         , prt5Filename  :: String
//         , prt5aNum      :: Int
//         , prt5bNum     :: Int
//         }-- n filename longName shortName units
//     | SMOKE3DDataFile
//         { smoke3dMeshNum   :: Int
//         , smoke3dFilename  :: String
//         , smoke3dLongName  :: String
//         , smoke3dShortName :: String
//         , smoke3dUnits     :: String
//         }-- n filename longName shortName units Int String String String String -- n filename longName shortName units
//     | PL3DDataFile
//         { pl3dTime      :: Double
//         , pl3dMeshNum   :: Int
//         , pl3dFilename  :: String
//         , pl3dEntries     :: [PL3DEntry]
//         }-- Double Int String [(String, String, String)] -- time n filename [(longName, shortName, units)]
//     | XYZDataFile
//         { xyzFilename  :: String
//         }
//     deriving Show

// getDataFileName dataFile = case dataFile of
//     SLCFDataFile {slcfFilename = filename} -> filename
//     BNDFDataFile {bndfFilename = filename} -> filename
//     PRT5DataFile {prt5Filename = filename} -> filename
//     SMOKE3DDataFile {smoke3dFilename = filename} -> filename
//     PL3DDataFile {pl3dFilename = filename} -> filename
//     XYZDataFile {xyzFilename = filename} -> filename

// data PL3DEntry = PL3DEntry
//     { pl3dEntryLongName  :: String
//     , pl3dEntryShortName :: String
//     , pl3dEntryUnits     :: String
//     } deriving Show

// -- dataFileMeshNum dataFileEntry = case dataFileEntry of
//     -- SLCFDataFile meshNum _ String String String ->
//     -- PRT5DataFile meshNum _ Int Int -- n filename aNum bNum
//     -- SMOKE3DDataFile meshNum _ String String String -- n filename longName shortName units
//     -- PL3DDataFile time meshNum String [(String, String, String)] -- time n filename [(longName, shortName, units)]

#[derive(Clone, Debug)]
pub struct DevcActEntry {
    pub name: String,
    pub n: i64,
    pub time: f64,
    pub x: f64,
}

#[derive(Clone, Debug)]
pub enum ObstVisEntry {
    ShowObst(i64, i64, f64),
    HideObst(i64, i64, f64),
}

// takeJust (Just x) = x

// parsePL3DEntry :: Parser PL3DEntry
// parsePL3DEntry = do
//     -- onlySpaces
//     _ <- char ' '
//     longName <- basicStringSpacesExtra
//     eol
//     _ <- onlySpaces
//     shortName <- basicStringExtra
//     eol
//     _ <- onlySpaces
//     units <- basicStringExtra
//     eol
//     return $ PL3DEntry longName shortName units

// clearLine :: Parser String
// clearLine = do
//     manyTill anyChar eol

// basicString :: Parser String
// basicString =
//     many1 (oneOf "0123456789qwertyuiopasdfghjklzxcvbnm:\\._QWERTYUIOPASDFGHJKLZXCVBNM-*")
//     <?> "basicString"

// basicStringExtra :: Parser String
// basicStringExtra =
//     many (oneOf "0123456789qwertyuiopasdfghjklzxcvbnm:\\._QWERTYUIOPASDFGHJKLZXCVBNM/-*")

// basicStringSpaces :: Parser String
// basicStringSpaces =
//     many (oneOf "0123456789qwertyuiopasdfghjklzxcvbnm:\\._ QWERTYUIOPASDFGHJKLZXCVBNM-*")

// basicStringSpacesExtra :: Parser String
// basicStringSpacesExtra =
//     many (oneOf "0123456789qwertyuiopasdfghjklzxcvbnm:\\._ QWERTYUIOPASDFGHJKLZXCVBNM/-*")
//     <?> "basicStringSpacesExtra"

// fullString :: Parser String
// fullString =
//     many (oneOf "0123456789qwertyuiopasdfghjklzxcvbnm:\\._, ()QWERTYUIOPASDFGHJKLZXCVBNM/-*")
//     <?> "basicStringSpacesExtra"

// deviceNameParser :: Parser String
// deviceNameParser =
//     many (oneOf "0123456789qwertyuiopasdfghjklzxcvbnm:\\._, ()QWERTYUIOPASDFGHJKLZXCVBNM/-*+")
//     <?> "Device Name"

// titleString :: Parser String
// titleString =
//     manyTill anyChar eol
//     <?> "Title String"

// basicStringComma :: Parser String
// basicStringComma =
//     many (oneOf "0123456789qwertyuiopasdfghjklzxcvbnm:\\.,_QWERTYUIOPASDFGHJKLZXCVBNM*")

// coords :: Parser (Int,Int,Int)
// coords = (,,)
//     <$> (char '(' *> onlySpaces *> intNum <* sep)
//     <*> (intNum <* sep)
//     <*> (intNum <* onlySpaces <* char ')')
//     <?> "Coordinates (i,j,k)"
//     where
//         sep :: Parser ()
//         sep = onlySpaces *> char ',' *> onlySpaces *> pure ()

#[cfg(test)]
mod tests {
    use super::*;
    use nom;
    use nom::error::ErrorKind::NoneOf;
    use std::error::Error;
    // In these tests Ok(remaining, result) is used to make sure that we have
    // consumed the input we expect to consume.

    //     #[test]
    //     fn boolean_examples() {
    //         assert_eq!(boolean(b"t"), Ok((&[][..], true)));
    //         assert_eq!(boolean(b"T"), Ok((&[][..], true)));
    //         assert_eq!(boolean(b"f"), Ok((&[][..], false)));
    //         assert_eq!(boolean(b"F"), Ok((&[][..], false)));
    //         assert_eq!(boolean(b".FALSE."), Ok((&[][..], false)));
    //         assert_eq!(boolean(b".TRUE."), Ok((&[][..], true)));
    //         assert_eq!(boolean(b".TRUE., "), Ok((b", ".as_ref(), true)));
    //     }

    //     #[test]
    //     fn int_examples() {
    //         assert_eq!(parse_int(b"-2"), Ok((&[][..], -2)));
    //         assert_eq!(parse_int(b"60."), Ok((&[][..], 60)));
    //         // assert_eq!(boolean(b"T"), Ok((&[][..], true)));
    //         // assert_eq!(boolean(b"f"), Ok((&[][..], false)));
    //         // assert_eq!(boolean(b"F"), Ok((&[][..], false)));
    //         // assert_eq!(boolean(b".FALSE."), Ok((&[][..], false)));
    //         // assert_eq!(boolean(b".TRUE."), Ok((&[][..], true)));
    //         // assert_eq!(boolean(b".TRUE., "), Ok((b", ".as_ref(), true)));
    //     }

    //     #[test]
    //     fn double_examples() {
    //         assert_eq!(parse_double(b"1E13"), Ok((&[][..], 1E13)));
    //         assert_eq!(parse_double(b"2.75E12"), Ok((&[][..], 2.75E12)));
    //         // assert_eq!(boolean(b"T"), Ok((&[][..], true)));
    //         // assert_eq!(boolean(b"f"), Ok((&[][..], false)));
    //         // assert_eq!(boolean(b"F"), Ok((&[][..], false)));
    //         // assert_eq!(boolean(b".FALSE."), Ok((&[][..], false)));
    //         // assert_eq!(boolean(b".TRUE."), Ok((&[][..], true)));
    //         // assert_eq!(boolean(b".TRUE., "), Ok((b", ".as_ref(), true)));
    //     }

    //     #[test]
    //     fn string_examples() {
    //         assert_eq!(
    //             quoted_string(b"\'hello\'"),
    //             Ok((&[][..], String::from("hello")))
    //         );
    //         // assert_eq!(boolean(b"T"), Ok((&[][..], true)));
    //         // assert_eq!(boolean(b"f"), Ok((&[][..], false)));
    //         // assert_eq!(boolean(b"F"), Ok((&[][..], false)));
    //         // assert_eq!(boolean(b".FALSE."), Ok((&[][..], false)));
    //         // assert_eq!(boolean(b".TRUE."), Ok((&[][..], true)));
    //         // assert_eq!(boolean(b".TRUE., "), Ok((b", ".as_ref(), true)));
    //     }

    //     #[test]
    //     fn parameter_name_examples() {
    //         assert_eq!(parameter_name(b"speed"), Ok((&[][..], "speed".to_string())));
    //         assert_eq!(parameter_name(b"s"), Ok((&[][..], "s".to_string())));
    //         // An empty string is not parsed
    //         assert_eq!(
    //             parameter_name(b""),
    //             Err(nom::Err::Error((b"".as_ref(), NoneOf)))
    //         );
    //         // Parameter name cannot start with a number
    //         assert_eq!(
    //             parameter_name(b"2speed"),
    //             Err(nom::Err::Error((b"2speed".as_ref(), NoneOf)))
    //         );
    //     }

    #[test]
    fn parse_smv_simple() {
        let result = parse_smv_file(include_str!("room_fire.smv"))
            .expect("smv parsing failed")
            .1;
        assert_eq!(result.title, "Single Couch Test Case".to_string());
        assert_eq!(result.chid, "room_fire".to_string());
    }

    #[test]
    fn parse_smv_blocks() {
        assert_eq!(
            many0(parse_smv_block)(include_str!("room_fire.smv"))
                .expect("smv parsing failed")
                .1
                .len(),
            91
        );

        // assert_eq!(boolean(b"T"), Ok((&[][..], true)));
        // assert_eq!(boolean(b"f"), Ok((&[][..], false)));
        // assert_eq!(boolean(b"F"), Ok((&[][..], false)));
        // assert_eq!(boolean(b".FALSE."), Ok((&[][..], false)));
        // assert_eq!(boolean(b".TRUE."), Ok((&[][..], true)));
        // assert_eq!(boolean(b".TRUE., "), Ok((b", ".as_ref(), true)));
    }

    #[test]
    fn parse_block() {
        let example = "BLOCKNAME1\n BLOCKCONTENT1\n BLOCKCONTENT2\n\n";
        let results = parse_smv_block(example).expect("smv parsing failed").1;
        assert_eq!(results.title, "BLOCKNAME1".to_string());
        assert_eq!(
            results.content,
            " BLOCKCONTENT1\n BLOCKCONTENT2\n\n".to_string()
        );
    }

    #[test]
    fn parse_many_blocks() {
        let example = "BLOCKNAME1\n BLOCKCONTENT1\n BLOCKCONTENT2\nBLOCKNAME2\n BLOCKCONTENT1\n BLOCKCONTENT2\n";
        let results = many0(parse_smv_block)(example)
            .expect("smv parsing failed")
            .1;
        assert_eq!(
            *results.get(0).unwrap(),
            SMVBlock {
                title: "BLOCKNAME1".to_string(),
                content: " BLOCKCONTENT1\n BLOCKCONTENT2\n".to_string(),
            }
        );
        assert_eq!(
            *results.get(1).unwrap(),
            SMVBlock {
                title: "BLOCKNAME2".to_string(),
                content: " BLOCKCONTENT1\n BLOCKCONTENT2\n".to_string(),
            }
        );
    }

    //     #[test]
    //     fn range_examples() {
    //         use std::collections::HashMap;
    //         assert_eq!(
    //             param_pos(b"(1:2)"),
    //             Ok((&[][..], ParamPos::OneDim(Range::TwoNumber(1_u64, 2_u64))))
    //         );
    //         // assert_eq!(boolean(b"T"), Ok((&[][..], true)));
    //         // assert_eq!(boolean(b"f"), Ok((&[][..], false)));
    //         // assert_eq!(boolean(b"F"), Ok((&[][..], false)));
    //         // assert_eq!(boolean(b".FALSE."), Ok((&[][..], false)));
    //         // assert_eq!(boolean(b".TRUE."), Ok((&[][..], true)));
    //         // assert_eq!(boolean(b".TRUE., "), Ok((b", ".as_ref(), true)));
    //     }

    //     #[test]
    //     fn array_examples() {
    //         use std::collections::HashMap;
    //         let mut group_spec = HashMap::new();
    //         group_spec.insert(
    //             "TEMPERATURES".to_string(),
    //             ParameterSpec::Array(ParameterSpecAtom::Double),
    //         );
    //         let mut value_map = HashMap::new();
    //         value_map.insert(vec![1], 273_f64.into());
    //         value_map.insert(vec![2], 274_f64.into());
    //         assert_eq!(
    //             parse_parameter(&group_spec, b"TEMPERATURES(1:2)=273, 274"),
    //             Ok((
    //                 &[][..],
    //                 Parameter {
    //                     name: "TEMPERATURES".to_string(),
    //                     value: ParameterValue::Array(
    //                         ParameterArray {
    //                             pos: ParamPos::OneDim(Range::TwoNumber(1_u64, 2_u64)),
    //                             values: value_map.clone(),
    //                         }
    //                     )
    //                 }
    //             ))
    //         );
    //         assert_eq!(
    //             parse_parameter(&group_spec, b"TEMPERATURES(1:2)=273 274"),
    //             Ok((
    //                 &[][..],
    //                 Parameter {
    //                     name: "TEMPERATURES".to_string(),
    //                     value: ParameterValue::Array(
    //                         ParameterArray {
    //                             pos: ParamPos::OneDim(Range::TwoNumber(1_u64, 2_u64)),
    //                             values: value_map
    //                         }
    //                     )
    //                 }
    //             ))
    //         );
    //         let mut value_map = HashMap::new();
    //         value_map.insert(vec![1], 273_f64.into());
    //         assert_eq!(
    //             parse_parameter(&group_spec, b"TEMPERATURES(1)=273"),
    //             Ok((
    //                 &[][..],
    //                 Parameter {
    //                     name: "TEMPERATURES".to_string(),
    //                     value: ParameterValue::Array(
    //                         ParameterArray {
    //                             pos: ParamPos::OneDim(Range::SingleNumber(1_u64)),
    //                             values: value_map
    //                         }
    //                     )
    //                 }
    //             ))
    //         );
    //         // assert_eq!(boolean(b"T"), Ok((&[][..], true)));
    //         // assert_eq!(boolean(b"f"), Ok((&[][..], false)));
    //         // assert_eq!(boolean(b"F"), Ok((&[][..], false)));
    //         // assert_eq!(boolean(b".FALSE."), Ok((&[][..], false)));
    //         // assert_eq!(boolean(b".TRUE."), Ok((&[][..], true)));
    //         // assert_eq!(boolean(b".TRUE., "), Ok((b", ".as_ref(), true)));
    //     }

    //     #[test]
    //     fn namelist_examples() {
    //         use std::collections::HashMap;
    //         let mut group_spec = HashMap::new();
    //         group_spec.insert(
    //             "TEMPERATURES".to_string(),
    //             ParameterSpec::Array(ParameterSpecAtom::Double),
    //         );
    //         let mut namelist_spec: NamelistSpec = HashMap::new();
    //         namelist_spec.insert("HEAD".to_string(), group_spec);
    //         let mut value_map = HashMap::new();
    //         value_map.insert(vec![1], 273_f64.into());
    //         value_map.insert(vec![2], 274_f64.into());
    //         let mut params_map = HashMap::new();
    //         params_map.insert("TEMPERATURES".to_string(), Parameter {
    //                 name: "TEMPERATURES".to_string(),
    //                 value: ParameterValue::Array(
    //                     ParameterArray {
    //                         pos: ParamPos::OneDim(Range::TwoNumber(1_u64, 2_u64)),
    //                         values: value_map,
    //                     }
    //                 ),
    //             });
    //         let expected = Some(Namelist {
    //             name: "HEAD".to_string(),
    //             parameters: params_map,
    //         });
    //         assert_eq!(
    //             parse_namelist(&namelist_spec, b"&HEAD TEMPERATURES(1:2)=273, 274 /"),
    //             Ok((&[][..], expected))
    //         );
    //     }

    //     // #[test]
    //     // fn namelistfile_examples() {
    //     //     use std::collections::HashMap;
    //     //     let mut group_spec = HashMap::new();
    //     //     group_spec.insert(
    //     //         "TEMPERATURES".to_string(),
    //     //         ParameterSpec::Array(ParameterSpecAtom::Double),
    //     //     );
    //     //     let mut namelist_spec: NamelistSpec = HashMap::new();
    //     //     namelist_spec.insert("HEAD".to_string(), group_spec);
    //     //     let expected = Namelist {
    //     //         name: "HEAD".to_string(),
    //     //         parameters: vec![Parameter {
    //     //             name: "TEMPERATURES".to_string(),
    //     //             value: ParameterValue::Array(
    //     //                 ParamPos::OneDim(Range::TwoNumber(1_u64, 2_u64)),
    //     //                 vec![273_f64.into(), 274_f64.into()],
    //     //             ),
    //     //         }],
    //     //     };
    //     //     assert_eq!(
    //     //         parse_namelist(&namelist_spec, b"&HEAD TEMPERATURES(1:2)=273, 274 /"),
    //     //         Ok((&[][..], expected))
    //     //     );
    //     // }
    //     #[test]
    //     fn float_check() {
    //         assert_eq!(parse_double(&b"1.1"[..]), Ok((&b""[..], 1.1)));
    //         assert_eq!(
    //             parse_double(&b"EX"[..]),
    //             Err(nom::Err::Error((&b"EX"[..], nom::error::ErrorKind::OneOf)))
    //         );
    //         assert_eq!(parse_double(&b"123E-02"[..]), Ok((&b""[..], 1.23)));
    //         assert_eq!(parse_double(&b"123K-01"[..]), Ok((&b"K-01"[..], 123.0)));
    //         assert_eq!(
    //             parse_double(&b"abc"[..]),
    //             Err(nom::Err::Error((&b"abc"[..], nom::error::ErrorKind::OneOf)))
    //         );
    //     }
}
