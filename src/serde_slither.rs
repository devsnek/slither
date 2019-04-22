use crate::{Agent, Value};
use crate::value::ObjectKey;

#[derive(Debug)]
pub struct Error;
impl std::fmt::Display for Error {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(fmt, "Error")
    }
}
impl std::error::Error for Error {}
impl serde::ser::Error for Error {
    fn custom<T>(_msg: T) -> Error
        where T: std::fmt::Display {
            Error
        }
}

type SerializerResult = Result<Value, Error>;

pub struct Serializer<'a> {
    agent: &'a Agent,
}

pub fn serialize<T>(agent: &Agent, v: &T) -> SerializerResult
    where T: ?Sized + serde::Serialize {
        v.serialize(Serializer { agent })
    }

impl<'a> serde::Serializer for Serializer<'a> {
    type Ok = Value;
    type Error = Error;

    type SerializeSeq = SequenceSerializer<'a>;
    type SerializeTuple = TupleSerializer<'a>;
    type SerializeTupleStruct = Self::SerializeTuple;
    type SerializeTupleVariant = Self;
    type SerializeMap = MapSerializer<'a>;
    type SerializeStruct = Self::SerializeMap;
    type SerializeStructVariant = Self;

    fn serialize_bool(self, v: bool) -> SerializerResult {
        Ok(Value::from(v))
    }

    fn serialize_i8(self, v: i8) -> SerializerResult {
        Ok(Value::from(f64::from(v)))
    }

    fn serialize_i16(self, v: i16) -> SerializerResult {
        Ok(Value::from(f64::from(v)))
    }

    fn serialize_i32(self, v: i32) -> SerializerResult {
        Ok(Value::from(f64::from(v)))
    }

    fn serialize_i64(self, v: i64) -> SerializerResult {
        Ok(Value::from(v as f64))
    }

    fn serialize_u8(self, v: u8) -> SerializerResult {
        Ok(Value::from(u32::from(v)))
    }

    fn serialize_u16(self, v: u16) -> SerializerResult {
        Ok(Value::from(u32::from(v)))
    }

    fn serialize_u32(self, v: u32) -> SerializerResult {
        Ok(Value::from(v))
    }

    fn serialize_u64(self, v: u64) -> SerializerResult {
        Ok(Value::from(v as f64))
    }

    fn serialize_f32(self, v: f32) -> SerializerResult {
        Ok(Value::from(f64::from(v)))
    }

    fn serialize_f64(self, v: f64) -> SerializerResult {
        Ok(Value::from(v))
    }

    fn serialize_char(self, v: char) -> SerializerResult {
        self.serialize_str(&v.to_string())
    }

    fn serialize_str(self, v: &str) -> SerializerResult {
        Ok(Value::from(v))
    }

    fn serialize_bytes(self, v: &[u8]) -> SerializerResult {
        Ok(Value::new_buffer_from_vec(self.agent, v.to_vec()))
    }

    // An absent optional is represented as the JSON `null`.
    fn serialize_none(self) -> SerializerResult {
        self.serialize_unit()
    }

    fn serialize_some<T>(self, value: &T) -> SerializerResult
    where
        T: ?Sized + serde::Serialize,
    {
        value.serialize(self)
    }

    fn serialize_unit(self) -> SerializerResult {
        Ok(Value::Null)
    }

    fn serialize_unit_struct(self, _name: &'static str) -> SerializerResult {
        self.serialize_unit()
    }

    // When serializing a unit variant (or any other kind of variant), formats
    // can choose whether to keep track of it by index or by name. Binary
    // formats typically use the index of the variant and human-readable formats
    // typically use the name.
    fn serialize_unit_variant(
        self,
        _name: &'static str,
        _variant_index: u32,
        variant: &'static str,
    ) -> SerializerResult {
        self.serialize_str(variant)
    }

    // As is done here, serializers are encouraged to treat newtype structs as
    // insignificant wrappers around the data they contain.
    fn serialize_newtype_struct<T>(
        self,
        _name: &'static str,
        value: &T,
    ) -> SerializerResult
    where
        T: ?Sized + serde::Serialize,
    {
        value.serialize(self)
    }

    // Note that newtype variant (and all of the other variant serialization
    // methods) refer exclusively to the "externally tagged" enum
    // representation.
    //
    // Serialize this to JSON in externally tagged form as `{ NAME: VALUE }`.
    fn serialize_newtype_variant<T>(
        self,
        _name: &'static str,
        _variant_index: u32,
        _variant: &'static str,
        _value: &T,
    ) -> SerializerResult
    where
        T: ?Sized + serde::Serialize,
    {
        panic!()
    }

    fn serialize_seq(self, len: Option<usize>) -> Result<Self::SerializeSeq, Error> {
        Ok(Self::SerializeSeq::new(self, len))
    }

    fn serialize_tuple(self, len: usize) -> Result<Self::SerializeTuple, Error> {
        Ok(Self::SerializeTuple::new(self, len))
    }

    // Tuple structs look just like sequences in JSON.
    fn serialize_tuple_struct(
        self,
        _name: &'static str,
        len: usize,
    ) -> Result<Self::SerializeTupleStruct, Error> {
        self.serialize_tuple(len)
    }

    // Tuple variants are represented in JSON as `{ NAME: [DATA...] }`. Again
    // this method is only responsible for the externally tagged representation.
    fn serialize_tuple_variant(
        self,
        _name: &'static str,
        _variant_index: u32,
        _variant: &'static str,
        _len: usize,
    ) -> Result<Self::SerializeTupleVariant, Error> {
        panic!()
    }

    // Maps are represented in JSON as `{ K: V, K: V, ... }`.
    fn serialize_map(self, len: Option<usize>) -> Result<Self::SerializeMap, Error> {
        Ok(Self::SerializeMap::new(self, len))
    }

    fn serialize_struct(
        self,
        _name: &'static str,
        len: usize,
    ) -> Result<Self::SerializeStruct, Error> {
        self.serialize_map(Some(len))
    }

    // Struct variants are represented in JSON as `{ NAME: { K: V, ... } }`.
    // This is the externally tagged representation.
    fn serialize_struct_variant(
        self,
        _name: &'static str,
        _variant_index: u32,
        _variant: &'static str,
        _len: usize,
    ) -> Result<Self::SerializeStructVariant, Error> {
        panic!()
    }
}

pub struct SequenceSerializer<'a> {
    agent: &'a Agent,
    values: Vec<Value>
}

impl<'a> SequenceSerializer<'a> {
    fn new(s: Serializer<'a>, _len: Option<usize>) -> SequenceSerializer<'a> {
        SequenceSerializer {
            agent: s.agent,
            values: Vec::new(),
        }
    }
}

impl<'a> serde::ser::SerializeSeq for SequenceSerializer<'a> {
    type Ok = Value;
    type Error = Error;

    fn serialize_element<T>(&mut self, value: &T) -> Result<(), Error>
    where
        T: ?Sized + serde::Serialize,
    {
        let ser = value.serialize(Serializer { agent: self.agent })?;
        self.values.push(ser);
        Ok(())
    }

    fn end(self) -> SerializerResult {
        Ok(Value::new_array_from_vec(self.agent, self.values))
    }
}

pub struct TupleSerializer<'a> {
    agent: &'a Agent,
    values: Vec<Value>,
}

impl<'a> TupleSerializer<'a> {
    fn new(s: Serializer<'a>, _len: usize) -> TupleSerializer<'a> {
        TupleSerializer {
            agent: s.agent,
            values: Vec::new(),
        }
    }
}

impl<'a> serde::ser::SerializeTuple for TupleSerializer<'a> {
    type Ok = Value;
    type Error = Error;

    fn serialize_element<T>(&mut self, value: &T) -> Result<(), Error>
    where
        T: ?Sized + serde::Serialize,
    {
        let ser = serialize(self.agent, value)?;
        self.values.push(ser);
        Ok(())
    }

    fn end(self) -> SerializerResult {
        Ok(Value::Tuple(self.values))
    }
}

impl<'a> serde::ser::SerializeTupleStruct for TupleSerializer<'a> {
    type Ok = Value;
    type Error = Error;

    fn serialize_field<T>(&mut self, value: &T) -> Result<(), Error>
    where
        T: ?Sized + serde::Serialize,
    {
        serde::ser::SerializeTuple::serialize_element(self, value)
    }

    fn end(self) -> SerializerResult {
        serde::ser::SerializeTuple::end(self)
    }
}

pub struct MapSerializer<'a> {
    agent: &'a Agent,
    object: Value,
    key: Option<ObjectKey>,
}

impl<'a> MapSerializer<'a> {
    fn new(s: Serializer<'a>, _len: Option<usize>) -> MapSerializer<'a> {
        MapSerializer {
            agent: s.agent,
            object: Value::new_object(s.agent.intrinsics.object_prototype.clone()),
            key: None,
        }
    }
}

impl<'a> serde::ser::SerializeMap for MapSerializer<'a> {
    type Ok = Value;
    type Error = Error;

    fn serialize_key<T>(&mut self, key: &T) -> Result<(), Error>
    where
        T: ?Sized + serde::Serialize,
    {
        let ser = serialize(self.agent, key)?;
        let key = ser.to_object_key(self.agent).unwrap();
        self.key = Some(key);
        Ok(())
    }

    fn serialize_value<T>(&mut self, value: &T) -> Result<(), Error>
    where
        T: ?Sized + serde::Serialize,
    {
        let value = serialize(self.agent, value)?;
        self.object.set(self.agent, self.key.take().unwrap(), value).unwrap();
        Ok(())
    }

    fn end(self) -> SerializerResult {
        Ok(self.object)
    }
}

impl<'a> serde::ser::SerializeStruct for MapSerializer<'a> {
    type Ok = Value;
    type Error = Error;

    fn serialize_field<T>(&mut self, key: &'static str, value: &T) -> Result<(), Error>
    where
        T: ?Sized + serde::Serialize,
    {
        serde::ser::SerializeMap::serialize_entry(self, key, value)
    }

    fn end(self) -> SerializerResult {
        serde::ser::SerializeMap::end(self)
    }
}

impl<'a> serde::ser::SerializeTupleVariant for Serializer<'a> {
    type Ok = Value;
    type Error = Error;

    fn serialize_field<T>(&mut self, _value: &T) -> Result<(), Error>
    where
        T: ?Sized + serde::Serialize,
    {
        panic!()
    }

    fn end(self) -> SerializerResult {
        panic!()
    }
}

impl<'a> serde::ser::SerializeStructVariant for Serializer<'a> {
    type Ok = Value;
    type Error = Error;

    fn serialize_field<T>(&mut self, _key: &'static str, _value: &T) -> Result<(), Error>
    where
        T: ?Sized + serde::Serialize,
    {
        panic!()
    }

    fn end(self) -> SerializerResult {
        panic!()
    }
}
