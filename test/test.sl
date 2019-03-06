import { print } from standard:debug;

const re = /'(?P<name>[^']+)'\s+\((?P<year>\d{4})\)/;

print(re.match("Not my favorite movie: 'Citizen Kane' (1941)."));
