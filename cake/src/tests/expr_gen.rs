use {
    crate::*,
    speculate::speculate,
};

speculate!{
    #[should_panic(expected = "Use skip() instead of empty string.")]
    it "parse string" {
        str("");
    }
}
