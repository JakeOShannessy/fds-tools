#[macro_use]
extern crate nom;
mod slice_parser;
mod smv_parser;
mod verification_tests;

pub use slice_parser::parse_slice_file;
pub use smv_parser::parse_smv_file;

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
