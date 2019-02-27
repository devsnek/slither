import { print } from standard:debug;
import { readFile, writeFile, removeFile, getMetadata } from standard:fs;

print(`hi how are you?`);
print(`hi how $('are you?')`);
print(`$('hi how') are you?`);
print(`hi $('how are') you$('?')`);
