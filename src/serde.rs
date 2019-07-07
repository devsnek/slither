use crate::value::{ObjectKey, ObjectKind, ValueType};
use crate::{Agent, Value};

#[derive(Debug)]
pub(crate) struct Error(String);
impl std::fmt::Display for Error {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(fmt, "Error")
    }
}
impl std::error::Error for Error {}
impl serde::ser::Error for Error {
    fn custom<T>(msg: T) -> Error
    where
        T: std::fmt::Display,
    {
        Error(format!("{}", msg))
    }
}
impl serde::de::Error for Error {
    fn custom<T>(msg: T) -> Error
    where
        T: std::fmt::Display,
    {
        Error(format!("{}", msg))
    }
}

type SerializerResult = Result<Value, Error>;

pub(crate) struct Serializer<'a> {
    agent: &'a Agent,
}

impl<'a> Serializer<'a> {
    pub(crate) fn new(agent: &'a Agent) -> Serializer<'a> {
        Serializer { agent }
    }
}

pub(crate) fn serialize<T>(agent: &Agent, v: &T) -> SerializerResult
where
    T: ?Sized + serde::Serialize,
{
    v.serialize(Serializer { agent })
}

impl<'a> serde::Serializer for Serializer<'a> {
    type Ok = Value;
    type Error = Error;

    type SerializeSeq = SequenceSerializer<'a>;
    type SerializeTuple = TupleSerializer<'a>;
    type SerializeTupleStruct = Self::SerializeTuple;
    type SerializeTupleVariant = Self::SerializeTuple;
    type SerializeMap = MapSerializer<'a>;
    type SerializeStruct = Self::SerializeMap;
    type SerializeStructVariant = Self::SerializeMap;

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

    fn serialize_none(self) -> SerializerResult {
        Ok(Value::Null)
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
    fn serialize_newtype_struct<T>(self, _name: &'static str, value: &T) -> SerializerResult
    where
        T: ?Sized + serde::Serialize,
    {
        value.serialize(self)
    }

    fn serialize_newtype_variant<T>(
        self,
        _name: &'static str,
        _variant_index: u32,
        _variant: &'static str,
        value: &T,
    ) -> SerializerResult
    where
        T: ?Sized + serde::Serialize,
    {
        serialize(self.agent, value)
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

    fn serialize_tuple_variant(
        self,
        _name: &'static str,
        _variant_index: u32,
        _variant: &'static str,
        len: usize,
    ) -> Result<Self::SerializeTupleVariant, Error> {
        self.serialize_tuple(len)
    }

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

    fn serialize_struct_variant(
        self,
        name: &'static str,
        _variant_index: u32,
        _variant: &'static str,
        len: usize,
    ) -> Result<Self::SerializeStructVariant, Error> {
        self.serialize_struct(name, len)
    }
}

pub(crate) struct SequenceSerializer<'a> {
    agent: &'a Agent,
    values: Vec<Value>,
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

pub(crate) struct TupleSerializer<'a> {
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

impl<'a> serde::ser::SerializeTupleVariant for TupleSerializer<'a> {
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

pub(crate) struct MapSerializer<'a> {
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
        self.object
            .set(self.agent, self.key.take().unwrap(), value)
            .unwrap();
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

impl<'a> serde::ser::SerializeStructVariant for MapSerializer<'a> {
    type Ok = Value;
    type Error = Error;

    fn serialize_field<T>(&mut self, key: &'static str, value: &T) -> Result<(), Error>
    where
        T: ?Sized + serde::Serialize,
    {
        let value = serialize(self.agent, value)?;
        self.object
            .set(self.agent, ObjectKey::from(key), value)
            .unwrap();
        Ok(())
    }

    fn end(self) -> SerializerResult {
        Ok(self.object)
    }
}

pub(crate) struct Deserializer<'a, 'de> {
    value: Value,
    agent: &'a Agent,
    phantom: std::marker::PhantomData<&'de ()>,
}

impl<'a, 'de> Deserializer<'a, 'de> {
    pub(crate) fn new(agent: &'a Agent, value: Value) -> Deserializer<'a, 'de> {
        Deserializer {
            value,
            agent,
            phantom: std::marker::PhantomData,
        }
    }
}

impl<'a, 'de> serde::Deserializer<'de> for Deserializer<'a, 'de> {
    type Error = Error;

    fn deserialize_any<V>(self, visitor: V) -> Result<V::Value, Error>
    where
        V: serde::de::Visitor<'de>,
    {
        match self.value {
            Value::Null => self.deserialize_unit(visitor),
            Value::Boolean(..) => self.deserialize_bool(visitor),
            Value::String(..) => self.deserialize_string(visitor),
            Value::Number(..) => self.deserialize_f64(visitor),
            Value::Symbol(..) => self.deserialize_ignored_any(visitor),
            Value::Object(ref o) => match o.kind {
                ObjectKind::Array(..) => self.deserialize_seq(visitor),
                ObjectKind::Buffer(..) => self.deserialize_byte_buf(visitor),
                ObjectKind::BuiltinFunction { .. } => self.deserialize_ignored_any(visitor),
                ObjectKind::BytecodeFunction { .. } => self.deserialize_ignored_any(visitor),
                _ => self.deserialize_map(visitor),
            },
            Value::Tuple(..) => self.deserialize_seq(visitor),
            _ => unreachable!(),
        }
    }

    fn deserialize_ignored_any<V>(self, visitor: V) -> Result<V::Value, Error>
    where
        V: serde::de::Visitor<'de>,
    {
        visitor.visit_unit()
    }

    fn deserialize_bool<V>(self, visitor: V) -> Result<V::Value, Error>
    where
        V: serde::de::Visitor<'de>,
    {
        visitor.visit_bool(match self.value {
            Value::Boolean(b) => b,
            _ => unreachable!(),
        })
    }

    fn deserialize_i8<V>(self, visitor: V) -> Result<V::Value, Error>
    where
        V: serde::de::Visitor<'de>,
    {
        visitor.visit_i8(match self.value {
            Value::Number(n) => n as i8,
            _ => unreachable!(),
        })
    }

    fn deserialize_i16<V>(self, visitor: V) -> Result<V::Value, Error>
    where
        V: serde::de::Visitor<'de>,
    {
        visitor.visit_i16(match self.value {
            Value::Number(n) => n as i16,
            _ => unreachable!(),
        })
    }

    fn deserialize_i32<V>(self, visitor: V) -> Result<V::Value, Error>
    where
        V: serde::de::Visitor<'de>,
    {
        visitor.visit_i32(match self.value {
            Value::Number(n) => n as i32,
            _ => unreachable!(),
        })
    }

    fn deserialize_i64<V>(self, visitor: V) -> Result<V::Value, Error>
    where
        V: serde::de::Visitor<'de>,
    {
        visitor.visit_i64(match self.value {
            Value::Number(n) => n as i64,
            _ => unreachable!(),
        })
    }

    fn deserialize_u8<V>(self, visitor: V) -> Result<V::Value, Error>
    where
        V: serde::de::Visitor<'de>,
    {
        visitor.visit_u8(match self.value {
            Value::Number(n) => n as u8,
            _ => unreachable!(),
        })
    }

    fn deserialize_u16<V>(self, visitor: V) -> Result<V::Value, Error>
    where
        V: serde::de::Visitor<'de>,
    {
        visitor.visit_u16(match self.value {
            Value::Number(n) => n as u16,
            _ => unreachable!(),
        })
    }

    fn deserialize_u32<V>(self, visitor: V) -> Result<V::Value, Error>
    where
        V: serde::de::Visitor<'de>,
    {
        visitor.visit_u32(match self.value {
            Value::Number(n) => n as u32,
            _ => unreachable!(),
        })
    }

    fn deserialize_u64<V>(self, visitor: V) -> Result<V::Value, Error>
    where
        V: serde::de::Visitor<'de>,
    {
        visitor.visit_u64(match self.value {
            Value::Number(n) => n as u64,
            _ => unreachable!(),
        })
    }

    fn deserialize_f32<V>(self, visitor: V) -> Result<V::Value, Error>
    where
        V: serde::de::Visitor<'de>,
    {
        visitor.visit_f32(match self.value {
            Value::Number(n) => n as f32,
            _ => unreachable!(),
        })
    }

    fn deserialize_f64<V>(self, visitor: V) -> Result<V::Value, Error>
    where
        V: serde::de::Visitor<'de>,
    {
        visitor.visit_f64(match self.value {
            Value::Number(n) => n,
            _ => unreachable!(),
        })
    }

    fn deserialize_char<V>(self, visitor: V) -> Result<V::Value, Error>
    where
        V: serde::de::Visitor<'de>,
    {
        visitor.visit_char(match self.value {
            Value::String(s) => s.chars().nth(0).unwrap(),
            _ => unreachable!(),
        })
    }

    fn deserialize_str<V>(self, visitor: V) -> Result<V::Value, Error>
    where
        V: serde::de::Visitor<'de>,
    {
        visitor.visit_string(match self.value {
            Value::String(s) => s,
            _ => unreachable!(),
        })
    }

    fn deserialize_string<V>(self, visitor: V) -> Result<V::Value, Error>
    where
        V: serde::de::Visitor<'de>,
    {
        self.deserialize_str(visitor)
    }

    fn deserialize_bytes<V>(self, _visitor: V) -> Result<V::Value, Error>
    where
        V: serde::de::Visitor<'de>,
    {
        panic!();
    }

    fn deserialize_byte_buf<V>(self, _visitor: V) -> Result<V::Value, Error>
    where
        V: serde::de::Visitor<'de>,
    {
        panic!();
    }

    fn deserialize_option<V>(self, _visitor: V) -> Result<V::Value, Error>
    where
        V: serde::de::Visitor<'de>,
    {
        panic!();
    }

    fn deserialize_unit<V>(self, visitor: V) -> Result<V::Value, Error>
    where
        V: serde::de::Visitor<'de>,
    {
        visitor.visit_unit()
    }

    fn deserialize_unit_struct<V>(self, _name: &'static str, visitor: V) -> Result<V::Value, Error>
    where
        V: serde::de::Visitor<'de>,
    {
        visitor.visit_unit()
    }

    fn deserialize_newtype_struct<V>(
        self,
        _name: &'static str,
        _visitor: V,
    ) -> Result<V::Value, Error>
    where
        V: serde::de::Visitor<'de>,
    {
        panic!();
    }

    fn deserialize_seq<V>(self, visitor: V) -> Result<V::Value, Error>
    where
        V: serde::de::Visitor<'de>,
    {
        visitor.visit_seq(ObjectDeserializer::new(&self))
    }

    fn deserialize_tuple<V>(self, _len: usize, visitor: V) -> Result<V::Value, Error>
    where
        V: serde::de::Visitor<'de>,
    {
        self.deserialize_seq(visitor)
    }

    fn deserialize_tuple_struct<V>(
        self,
        _name: &'static str,
        _len: usize,
        _visitor: V,
    ) -> Result<V::Value, Error>
    where
        V: serde::de::Visitor<'de>,
    {
        unreachable!();
    }

    fn deserialize_map<V>(self, visitor: V) -> Result<V::Value, Error>
    where
        V: serde::de::Visitor<'de>,
    {
        visitor.visit_map(ObjectDeserializer::new(&self))
    }

    fn deserialize_struct<V>(
        self,
        _name: &'static str,
        _fields: &'static [&'static str],
        _visitor: V,
    ) -> Result<V::Value, Error>
    where
        V: serde::de::Visitor<'de>,
    {
        panic!();
    }

    fn deserialize_enum<V>(
        self,
        _name: &'static str,
        _variants: &'static [&'static str],
        _visitor: V,
    ) -> Result<V::Value, Error>
    where
        V: serde::de::Visitor<'de>,
    {
        panic!();
    }

    fn deserialize_identifier<V>(self, _visitor: V) -> Result<V::Value, Error>
    where
        V: serde::de::Visitor<'de>,
    {
        panic!();
    }
}

struct ObjectDeserializer<'a, 'de> {
    de: &'a Deserializer<'a, 'de>,
    keys: Vec<ObjectKey>,
    index: usize,
}

impl<'a, 'de> ObjectDeserializer<'a, 'de> {
    fn new(de: &'a Deserializer<'a, 'de>) -> Self {
        ObjectDeserializer {
            de,
            keys: de.value.keys(de.agent).unwrap(),
            index: 0,
        }
    }
}

impl<'a, 'de> serde::de::SeqAccess<'de> for ObjectDeserializer<'a, 'de> {
    type Error = Error;

    fn next_element_seed<T>(&mut self, seed: T) -> Result<Option<T::Value>, Error>
    where
        T: serde::de::DeserializeSeed<'de>,
    {
        if self.index >= self.keys.len() {
            return Ok(None);
        }
        let value = self
            .de
            .value
            .get(self.de.agent, self.keys[self.index].clone())
            .unwrap();
        self.index += 1;
        seed.deserialize(Deserializer::new(self.de.agent, value))
            .map(Some)
    }
}

impl<'a, 'de> serde::de::MapAccess<'de> for ObjectDeserializer<'a, 'de> {
    type Error = Error;

    fn next_key_seed<K>(&mut self, seed: K) -> Result<Option<K::Value>, Error>
    where
        K: serde::de::DeserializeSeed<'de>,
    {
        loop {
            if self.index >= self.keys.len() {
                return Ok(None);
            }
            if let ObjectKey::Symbol(..) = self.keys[self.index] {
                self.index += 1;
                continue;
            }
            let value = self
                .de
                .value
                .get(self.de.agent, self.keys[self.index].clone())
                .unwrap();
            match value.type_of() {
                ValueType::Function | ValueType::Symbol => {
                    self.index += 1;
                }
                _ => {
                    return seed
                        .deserialize(Deserializer::new(
                            self.de.agent,
                            match self.keys[self.index] {
                                ObjectKey::Number(n) => Value::String(n.to_string()),
                                ObjectKey::String(ref s) => Value::String(s.clone()),
                                _ => unreachable!(),
                            },
                        ))
                        .map(Some)
                }
            }
        }
    }

    fn next_value_seed<V>(&mut self, seed: V) -> Result<V::Value, Error>
    where
        V: serde::de::DeserializeSeed<'de>,
    {
        let index = self.index;
        self.index += 1;
        let value = self
            .de
            .value
            .get(self.de.agent, self.keys[index].clone())
            .unwrap();
        seed.deserialize(Deserializer::new(self.de.agent, value))
    }
}
