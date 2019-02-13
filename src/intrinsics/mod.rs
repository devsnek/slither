mod array_prototype;
mod boolean_prototype;
mod function_prototype;
mod number_prototype;
mod object_prototype;
pub mod promise;
mod promise_prototype;
mod string_prototype;

pub use array_prototype::create_array_prototype;
pub use boolean_prototype::create_boolean_prototype;
pub use function_prototype::create_function_prototype;
pub use number_prototype::create_number_prototype;
pub use object_prototype::create_object_prototype;
pub use promise::create_promise;
pub use promise_prototype::create_promise_prototype;
pub use string_prototype::create_string_prototype;
