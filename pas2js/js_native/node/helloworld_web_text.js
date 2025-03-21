
const http = require('http');

const hostname = '127.0.0.1';
const port = 3000;

const server = http.createServer((req, res) => {
  res.writeHead(200, { 'Content-Type': 'text/plain' });
  res.write('Hello World 1!\n');
  res.write('Hello World 2!\n');
  res.write('Hello World 3!\n');
  res.end(); // Schließt die Antwort ab

  res.writeHead(200, { 'Content-Type': 'text/plain' });
  res.write('Hello World 1!\n');
  res.write('Hello World 2!\n');
  res.write('Hello World 3!\n');
  res.end(); // Schließt die Antwort ab
});

server.listen(port, hostname, () => {
  console.log(`Server starte an http://${hostname}:${port}/`);
  console.log(`Zum ausführen im Browser bei Adrresse folgendes eingeben: "http://localhost:3000"`);
});

