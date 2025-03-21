const http = require('http');

const server = http.createServer((req, res) => {
  res.writeHead(200, {'Content-Type': 'text/html; charset=utf-8'});
  res.write('<!DOCTYPE html>');
  res.write('<html>');
  res.write('<head><title>Meine JS Seite</title></head>');
  res.write('<body>');
  res.write('<h1>Willkommen auf meiner Seite!</h1>');
  res.write('<p>Dies ist ein Beispiel für HTML-Ausgabe mit Node.js.</p>');
  res.write('</body>');
  res.write('</html>');
  res.end();
});

server.listen(3000, () => {
  console.log('Server läuft auf http://localhost:3000/');
});


