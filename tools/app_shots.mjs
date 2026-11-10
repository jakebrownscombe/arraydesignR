// Screenshot the running app for the README and the site.
// Usage: node app_shots.mjs <app-url> <out-dir>
// Drives headless Chrome through the Chrome DevTools Protocol, the pattern of
// detqc/tools/screenshot_clip.mjs. The window is 1400 x 900 CSS pixels.
// The app opens with no receivers, so the script presses Generate (20
// receivers on the grid, seed 1) and then Compute coverage before it shoots.
// Writes four PNGs to <out-dir>: the Map tab after Compute coverage on the
// array detection probability layer, the Performance tab on that coverage,
// the Range model tab, and the Simulation tab after one run, clipped from the
// result tiles to the track map. Prints one JSON line per shot. When either
// press gives no result, it exits 1 and writes no shot from that press on.
// Used by tools/site_figures.R.
import { spawn } from 'node:child_process';
import fs from 'node:fs';
import os from 'node:os';
import path from 'node:path';

const [, , url, outDir] = process.argv;
const W = 1400, H = 900;
const CHROME = process.env.CHROME ||
  '/Applications/Google Chrome.app/Contents/MacOS/Google Chrome';
const PORT = 9300 + Math.floor(Math.random() * 600);
const prof = fs.mkdtempSync(path.join(os.tmpdir(), 'adr_shot_'));
const chrome = spawn(CHROME, ['--headless=new', '--disable-gpu', '--no-first-run',
  '--hide-scrollbars', `--user-data-dir=${prof}`, `--remote-debugging-port=${PORT}`,
  `--window-size=${W},${H}`, 'about:blank'], { stdio: 'ignore' });
const sleep = ms => new Promise(r => setTimeout(r, ms));
let targets;
for (let i = 0; i < 300 && !targets; i++) {
  try { targets = await (await fetch(`http://127.0.0.1:${PORT}/json/list`)).json(); } catch { await sleep(200); }
}
const ws = new WebSocket(targets.find(t => t.type === 'page').webSocketDebuggerUrl);
await new Promise(r => ws.addEventListener('open', r));
let seq = 0; const pending = new Map();
ws.addEventListener('message', ev => {
  const m = JSON.parse(ev.data);
  if (m.id && pending.has(m.id)) { pending.get(m.id)(m); pending.delete(m.id); }
});
const send = (method, params = {}) => new Promise(r => {
  const i = ++seq; pending.set(i, r); ws.send(JSON.stringify({ id: i, method, params }));
});
const js = async e => (await send('Runtime.evaluate',
  { expression: e, returnByValue: true, awaitPromise: true })).result.result.value;

// Waits until Shiny has been idle for 2 s, up to `max` ms.
const idle = async (max = 120000) => {
  let quiet = 0;
  for (let t = 0; t < max; t += 250) {
    const busy = await js("document.documentElement.classList.contains('shiny-busy') || " +
      "[...document.querySelectorAll('button.bslib-task-button')].some(b => b.disabled)");
    quiet = busy ? 0 : quiet + 250;
    if (quiet >= 2000) return t;
    await sleep(250);
  }
  return -1;
};
const shot = async (name, clip) => {
  const params = { format: 'png' };
  if (clip) { params.clip = { ...clip, scale: 1 }; params.captureBeyondViewport = true; }
  const res = await send('Page.captureScreenshot', params);
  const out = path.join(outDir, name);
  fs.writeFileSync(out, Buffer.from(res.result.data, 'base64'));
  console.log(JSON.stringify({ out: name, clip: clip || 'viewport', bytes: fs.statSync(out).size }));
};
// Closes Chrome, removes its profile and exits with `code`.
const stop = async code => {
  chrome.kill('SIGKILL');
  await sleep(500);
  fs.rmSync(prof, { recursive: true, force: true, maxRetries: 5 });
  process.exit(code);
};
const tab = async label => {
  await js(`[...document.querySelectorAll('.navbar .nav-link')].find(a => a.textContent.trim() === '${label}').click()`);
  await sleep(1500);
  await idle();
};

await send('Page.enable');
await send('Emulation.setDeviceMetricsOverride', { width: W, height: H, deviceScaleFactor: 1, mobile: false });
await send('Emulation.setEmulatedMedia', { features: [{ name: 'prefers-color-scheme', value: 'light' }] });
await send('Page.navigate', { url });
await sleep(4000);
await idle();

// ---- Map tab after Generate and Compute coverage ----
await js("document.getElementById('array-generate').click()");
await sleep(1500);
await idle();
await js("document.getElementById('coverage-compute').click()");
await sleep(1500);
await idle();
await js("(() => { const r = document.querySelector('input[name=\"map-layer\"][value=\"p_array\"]');" +
  " if (!r.checked) r.click(); })()");
await sleep(2500);
await idle();
await sleep(3000); // base map tiles
const status = await js("document.getElementById('coverage-banner_map').innerText + ' | ' + " +
  "document.getElementById('map-caption').innerText");
console.log(JSON.stringify({ map_status: status }));
// A Compute coverage press that gave no result leaves the banner without the
// headline ("No coverage yet ..." or a refusal). The run stops before the
// first shot, so tools/site_figures.R copies nothing.
if (!status.includes('of the water covered')) {
  console.error(`app_shots: Compute coverage gave no result; no PNG written. Map status: ${status.trim()}`);
  await stop(1);
}
await shot('app_map.png');

// ---- Performance tab on the same coverage ----
await tab('Performance');
await sleep(1500);
await shot('app_performance.png');

// ---- Range model tab ----
await tab('Range model');
await shot('app_range_model.png');

// ---- Simulation tab after one run ----
await tab('Simulation');
await js("document.getElementById('score-score').click()");
await sleep(2000);
await idle(300000);
await sleep(2000);
// The page scrolls inside the main column, so the window is made tall enough
// to hold the whole tab and the clip is taken from the main column: the
// result tiles down to the bottom of the track map card.
await send('Emulation.setDeviceMetricsOverride', { width: W, height: 3000, deviceScaleFactor: 1, mobile: false });
await sleep(2500);
await idle();
const clip = await js(`(() => {
  const tiles = document.getElementById('score-boxes').firstElementChild;
  if (!tiles) return null;
  const t = tiles.getBoundingClientRect();
  const m = document.getElementById('score-map').closest('.card').getBoundingClientRect();
  return { x: m.left - 12, y: t.top - 12, width: m.width + 24, height: m.bottom - t.top + 24 }; })()`);
const line = await js("document.getElementById('score-line').innerText");
console.log(JSON.stringify({ simulation_line: line }));
// A press that gave no result leaves the line empty and no result tiles. The
// run stops without the fourth shot, so tools/site_figures.R copies nothing.
if (!line || !line.trim() || !clip) {
  const banner = await js("document.getElementById('score-banner').innerText");
  console.error(`app_shots: Run simulation gave no result; no app_simulation.png written. ` +
    `Banner: ${banner.trim()}`);
  await stop(1);
}
await shot('app_simulation.png', clip);
await stop(0);
