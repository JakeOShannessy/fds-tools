#[macro_use]
extern crate nom;
mod verification_tests;
mod smv_parser;

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
