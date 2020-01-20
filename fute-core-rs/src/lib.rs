#[macro_use]
extern crate nom;
mod smv_parser;
mod slice_parser;
mod verification_tests;

pub use smv_parser::parse_smv_file;
pub use slice_parser::parse_slice_file;

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
