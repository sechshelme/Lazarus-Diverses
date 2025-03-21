const http = require('http');
const fs = require('fs');

let clickCount = 0;

// Lade den Zählerstand beim Start
try {
  clickCount = parseInt(fs.readFileSync('clickCount.txt', 'utf8'));
} catch (error) {
  console.log('Keine vorherige Zählung gefunden, starte bei 0');
}

const server = http.createServer((req, res) => {
  if (req.url === '/click') {
    clickCount++;
    fs.writeFileSync('clickCount.txt', clickCount.toString());
    res.writeHead(200, { 'Content-Type': 'text/plain' });
    res.end(`Klicks: ${clickCount}`);
  } else {
      res.writeHead(200, {'Content-Type': 'text/html; charset=utf-8'});
      res.end(`
      <html>
        <body>
          <h1>Klick-Zähler</h1>
          <button onclick="handleClick()">Klick mich!</button>
          <p id="count">Klicks: ${clickCount}</p>
          <script>
            function handleClick() {
              fetch('/click')
                .then(response => response.text())
                .then(data => document.getElementById('count').textContent = data);
            }
          </script>
        </body>
      </html>
    `);
  }
});

server.listen(3000, () => {
  console.log('Server läuft auf http://localhost:3000/');
});

