mod api;
mod error;

#[cfg(test)]
mod tests {
    use crate::api::{Assembler, I64};
    use crate::error::Result;

    #[test]
    fn api() -> Result<()> {
        // fn iterative_fib(n: i64) -> (r: i64) {
        //     if n == 0 {
        //         r = 0;
        //     } else {
        //         n = n - 1;
        //         let a: i64 = 0;
        //         r = 1;
        //         while n != 0 {
        //             let t: i64 = r;
        //             r = r + a;
        //             a = t;
        //             n = n - 1;
        //         }
        //     }
        // }
        let assembler = Assembler::default();
        let mut builder = assembler
            .new_func_builder("iterative_fib")
            .param("n", I64)
            .ret("r", I64);
        let mut fn_body = builder.enter_block();
        let mut if_body =
            fn_body.if_else(fn_body.equal(fn_body.id("n")?, fn_body.lit("0", I64))?)?;
        if_body.assign("r", if_body.lit("0", I64))?;
        if_body.enter_else();
        if_body.assign(
            "n",
            if_body.subtract(if_body.id("n")?, if_body.lit("1", I64))?,
        )?;
        if_body.declare_as("a", if_body.lit("0", I64))?;
        if_body.assign("r", if_body.lit("1", I64))?;
        let mut while_body =
            if_body.while_loop(if_body.not_equal(if_body.id("n")?, if_body.lit("0", I64))?)?;
        while_body.declare_as("t", while_body.id("r")?)?;
        while_body.assign(
            "r",
            while_body.add(while_body.id("r")?, while_body.id("a")?)?,
        )?;
        while_body.assign("a", while_body.id("t")?)?;
        while_body.assign(
            "n",
            while_body.subtract(while_body.id("n")?, while_body.lit("1", I64))?,
        )?;
        while_body.leave()?;
        if_body.leave()?;
        let gen_func = fn_body.build();
        Ok(())
    }
}
