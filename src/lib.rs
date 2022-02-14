mod api;
mod error;

#[cfg(test)]
mod tests {
    use crate::api::{Assembler, I64};
    use crate::error::Result;

    #[test]
    fn iterative_fib() -> Result<()> {
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

        fn_body.if_block(
            |cond| cond.eq(cond.id("n")?, cond.lit("0", I64)),
            |t| {
                t.assign("r", t.lit("0", I64))?;
                Ok(())
            },
            |e| {
                e.assign("n", e.sub(e.id("n")?, e.lit("1", I64))?)?;
                e.declare_as("a", e.lit("0", I64))?;
                e.assign("r", e.lit("1", I64))?;
                e.while_block(
                    |cond| cond.ne(cond.id("n")?, cond.lit("0", I64)),
                    |w| {
                        w.declare_as("t", w.id("r")?)?;
                        w.assign("r", w.add(w.id("r")?, w.id("a")?)?)?;
                        w.assign("a", w.id("t")?)?;
                        w.assign("n", w.sub(w.id("n")?, w.lit("1", I64))?)?;
                        Ok(())
                    },
                )?;
                Ok(())
            },
        )?;

        let gen_func = fn_body.build();
        println!("{}", gen_func);
        Ok(())
    }
}
