#[macro_use]
extern crate nom;
mod smv_parser;
mod verification_tests;

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
