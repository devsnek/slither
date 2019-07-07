use crate::value::Args;
use crate::{Agent, Value};
use serde::Serialize;
use std::collections::HashMap;

// https://github.com/sfackler/serde-transcode
// MIT License
fn d2s<D, S>(d: D) -> S
where
    D: serde::de::Error,
    S: serde::ser::Error,
{
    S::custom(d.to_string())
}

fn s2d<S, D>(s: S) -> D
where
    S: serde::ser::Error,
    D: serde::de::Error,
{
    D::custom(s.to_string())
}

struct Transcoder<D>(std::cell::RefCell<Option<D>>);

impl<'de, D> Transcoder<D>
where
    D: serde::Deserializer<'de>,
{
    pub fn new(d: D) -> Transcoder<D> {
        Transcoder(std::cell::RefCell::new(Some(d)))
    }
}

impl<'de, D> serde::Serialize for Transcoder<D>
where
    D: serde::Deserializer<'de>,
{
    fn serialize<S>(&self, s: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        self.0
            .borrow_mut()
            .take()
            .unwrap()
            .deserialize_any(Visitor(s))
            .map_err(d2s)
    }
}

struct Visitor<S>(S);

impl<'de, S> serde::de::Visitor<'de> for Visitor<S>
where
    S: serde::Serializer,
{
    type Value = S::Ok;
    fn expecting(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(fmt, "any value")
    }

    fn visit_bool<E>(self, v: bool) -> Result<S::Ok, E>
    where
        E: serde::de::Error,
    {
        self.0.serialize_bool(v).map_err(s2d)
    }

    fn visit_i8<E>(self, v: i8) -> Result<S::Ok, E>
    where
        E: serde::de::Error,
    {
        self.0.serialize_i8(v).map_err(s2d)
    }

    fn visit_i16<E>(self, v: i16) -> Result<S::Ok, E>
    where
        E: serde::de::Error,
    {
        self.0.serialize_i16(v).map_err(s2d)
    }

    fn visit_i32<E>(self, v: i32) -> Result<S::Ok, E>
    where
        E: serde::de::Error,
    {
        self.0.serialize_i32(v).map_err(s2d)
    }

    fn visit_i64<E>(self, v: i64) -> Result<S::Ok, E>
    where
        E: serde::de::Error,
    {
        self.0.serialize_i64(v).map_err(s2d)
    }

    fn visit_u8<E>(self, v: u8) -> Result<S::Ok, E>
    where
        E: serde::de::Error,
    {
        self.0.serialize_u8(v).map_err(s2d)
    }

    fn visit_u16<E>(self, v: u16) -> Result<S::Ok, E>
    where
        E: serde::de::Error,
    {
        self.0.serialize_u16(v).map_err(s2d)
    }

    fn visit_u32<E>(self, v: u32) -> Result<S::Ok, E>
    where
        E: serde::de::Error,
    {
        self.0.serialize_u32(v).map_err(s2d)
    }

    fn visit_u64<E>(self, v: u64) -> Result<S::Ok, E>
    where
        E: serde::de::Error,
    {
        self.0.serialize_u64(v).map_err(s2d)
    }

    fn visit_f32<E>(self, v: f32) -> Result<S::Ok, E>
    where
        E: serde::de::Error,
    {
        self.0.serialize_f32(v).map_err(s2d)
    }

    fn visit_f64<E>(self, v: f64) -> Result<S::Ok, E>
    where
        E: serde::de::Error,
    {
        self.0.serialize_f64(v).map_err(s2d)
    }

    fn visit_char<E>(self, v: char) -> Result<S::Ok, E>
    where
        E: serde::de::Error,
    {
        self.0.serialize_char(v).map_err(s2d)
    }

    fn visit_str<E>(self, v: &str) -> Result<S::Ok, E>
    where
        E: serde::de::Error,
    {
        self.0.serialize_str(v).map_err(s2d)
    }

    fn visit_string<E>(self, v: String) -> Result<S::Ok, E>
    where
        E: serde::de::Error,
    {
        self.0.serialize_str(&v).map_err(s2d)
    }

    fn visit_unit<E>(self) -> Result<S::Ok, E>
    where
        E: serde::de::Error,
    {
        self.0.serialize_unit().map_err(s2d)
    }

    fn visit_none<E>(self) -> Result<S::Ok, E>
    where
        E: serde::de::Error,
    {
        self.0.serialize_none().map_err(s2d)
    }

    fn visit_some<D>(self, d: D) -> Result<S::Ok, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        self.0.serialize_some(&Transcoder::new(d)).map_err(s2d)
    }

    fn visit_newtype_struct<D>(self, d: D) -> Result<S::Ok, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        self.0
            .serialize_newtype_struct("<unknown>", &Transcoder::new(d))
            .map_err(s2d)
    }

    fn visit_seq<V>(self, mut v: V) -> Result<S::Ok, V::Error>
    where
        V: serde::de::SeqAccess<'de>,
    {
        let mut s = self.0.serialize_seq(v.size_hint()).map_err(s2d)?;
        while let Some(()) = v.next_element_seed(SeqSeed(&mut s))? {}
        serde::ser::SerializeSeq::end(s).map_err(s2d)
    }

    fn visit_map<V>(self, mut v: V) -> Result<S::Ok, V::Error>
    where
        V: serde::de::MapAccess<'de>,
    {
        let mut s = self.0.serialize_map(v.size_hint()).map_err(s2d)?;
        while let Some(()) = v.next_key_seed(KeySeed(&mut s))? {
            v.next_value_seed(ValueSeed(&mut s))?;
        }
        serde::ser::SerializeMap::end(s).map_err(s2d)
    }

    fn visit_bytes<E>(self, v: &[u8]) -> Result<S::Ok, E>
    where
        E: serde::de::Error,
    {
        self.0.serialize_bytes(v).map_err(s2d)
    }

    fn visit_byte_buf<E>(self, v: Vec<u8>) -> Result<S::Ok, E>
    where
        E: serde::de::Error,
    {
        self.0.serialize_bytes(&v).map_err(s2d)
    }
}

struct SeqSeed<'a, S: 'a>(&'a mut S);

impl<'de, 'a, S> serde::de::DeserializeSeed<'de> for SeqSeed<'a, S>
where
    S: serde::ser::SerializeSeq,
{
    type Value = ();

    fn deserialize<D>(self, deserializer: D) -> Result<(), D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        self.0
            .serialize_element(&Transcoder::new(deserializer))
            .map_err(s2d)
    }
}

struct KeySeed<'a, S: 'a>(&'a mut S);

impl<'de, 'a, S> serde::de::DeserializeSeed<'de> for KeySeed<'a, S>
where
    S: serde::ser::SerializeMap,
{
    type Value = ();

    fn deserialize<D>(self, deserializer: D) -> Result<(), D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        self.0
            .serialize_key(&Transcoder::new(deserializer))
            .map_err(s2d)
    }
}

struct ValueSeed<'a, S: 'a>(&'a mut S);

impl<'de, 'a, S> serde::de::DeserializeSeed<'de> for ValueSeed<'a, S>
where
    S: serde::ser::SerializeMap,
{
    type Value = ();

    fn deserialize<D>(self, deserializer: D) -> Result<(), D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        self.0
            .serialize_value(&Transcoder::new(deserializer))
            .map_err(s2d)
    }
}
// End of serde-transcode

fn json_parse(args: Args) -> Result<Value, Value> {
    if let Value::String(s) = &args[0] {
        let mut deserializer = serde_json::Deserializer::from_str(s);
        let serializer = crate::serde::Serializer::new(args.agent());
        Transcoder::new(&mut deserializer)
            .serialize(serializer)
            .map_err(|e| Value::new_error(args.agent(), &e.to_string()))
    } else {
        Err(Value::new_error(args.agent(), "str must be a string"))
    }
}

fn json_stringify(args: Args) -> Result<Value, Value> {
    let deserializer = crate::serde::Deserializer::new(args.agent(), args[0].clone());
    let mut out = Vec::new();
    let mut serializer = serde_json::Serializer::new(&mut out);
    Transcoder::new(deserializer)
        .serialize(&mut serializer)
        .map_err(|e| Value::new_error(args.agent(), &e.to_string()))?;
    let string = unsafe { String::from_utf8_unchecked(out) };
    Ok(Value::from(string))
}

pub(crate) fn create(agent: &Agent) -> HashMap<String, Value> {
    let mut module = HashMap::new();
    module.insert(
        "parse".to_string(),
        Value::new_builtin_function(agent, json_parse, false),
    );
    module.insert(
        "stringify".to_string(),
        Value::new_builtin_function(agent, json_stringify, false),
    );

    module
}
