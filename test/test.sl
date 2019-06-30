import { parse, stringify } from standard:json;

print(parse(`{
  "a": 1,
  "b": null,
  "c": ["hi", [1, 2, 3]]
}`));

print(stringify({
  a: 1,
  b: null,
  d: () => 1,
  c: ['hi', (1, 2, 3)],
  e: :owo,
  [:owo]: 'OWO',
}));
