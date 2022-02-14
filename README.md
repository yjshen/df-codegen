# JIT Code Generation for Arrow-DataFusion

This project is a code generation tool for the [Arrow-DataFusion](https://github.com/apache/arrow-datafusion) project.

With JIT codegen, we could generate specific code for each query to reduce branching overhead from the generalized interpret mode execution.
Furthermore, we could reduce the memory footprint during the execution by chaining multiple Arrow compute kernels together and reusing the intermediate vectors.

I've just finished the proof of concept and will first try to accelerate row and columnar data transformation introduced in https://github.com/apache/arrow-datafusion/pull/1782.


## Development

TODOs:
- [ ] Cleanup ret values for functions and relax the restrictions on the return type
- [ ] Function register and reuse 
- [ ] Hook JIT codegen with DataFusion `RuntimeEnv`
- [ ] ...