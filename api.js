const jsonServer = require('json-server');

// Returns an express server
const server = jsonServer.create();

// Set default middleware
server.use(jsonServer.defaults());

const router = jsonServer.router('db.json');
server.use(router);

console.log('Listening at http://127.0.0.1:4000');
server.listen(4000);