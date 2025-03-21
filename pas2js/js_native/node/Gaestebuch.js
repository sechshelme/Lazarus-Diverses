const http = require('http');
const fs = require('fs');

let guestbook = [];

// Lade Gästebucheinträge beim Start
try {
  guestbook = JSON.parse(fs.readFileSync('guestbook.json', 'utf8'));
} catch (error) {
  console.log('Keine vorherigen Einträge gefunden, starte mit leerem Gästebuch');
}

const server = http.createServer((req, res) => {
  if (req.method === 'POST' && req.url === '/submit') {
    let body = '';
    req.on('data', chunk => {
      body += chunk.toString();
    });
    req.on('end', () => {
      const { name, message } = JSON.parse(body);
      guestbook.push({ name, message, date: new Date().toISOString() });
      fs.writeFileSync('guestbook.json', JSON.stringify(guestbook));
      res.writeHead(200, {'Content-Type': 'text/html; charset=utf-8'});
      res.end('Eintrag gespeichert');
    });
  } else {
    res.writeHead(200, {'Content-Type': 'text/html; charset=utf-8'});
    res.end(`
      <html>
        <body>
          <h1>Gästebuch</h1>
          <form onsubmit="handleSubmit(event)">
            <input type="text" id="name" placeholder="Name" required><br>
            <textarea id="message" placeholder="Nachricht" required></textarea><br>
            <button type="submit">Eintragen</button>
          </form>
          <h2>Einträge:</h2>
          <div id="entries">
            ${guestbook.map(entry => `
              <p><strong>${entry.name}</strong> (${entry.date}):<br>${entry.message}</p>
            `).join('')}
          </div>
          <script>
            function handleSubmit(event) {
              event.preventDefault();
              const name = document.getElementById('name').value;
              const message = document.getElementById('message').value;
              fetch('/submit', {
                method: 'POST',
                body: JSON.stringify({ name, message })
              }).then(() => location.reload());
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

