open Lwt.Syntax
open Types

(* Embedded HTML/CSS/JS SPA *)
let index_html = {|<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>Experience Agent</title>
<style>
  * { margin: 0; padding: 0; box-sizing: border-box; }
  body {
    font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, monospace;
    background: #1a1a2e; color: #e0e0e0; height: 100vh; display: flex;
    flex-direction: column;
  }
  header {
    background: #16213e; padding: 12px 20px; border-bottom: 1px solid #0f3460;
    display: flex; align-items: center; gap: 12px;
  }
  header h1 { font-size: 16px; color: #e94560; }
  header span { font-size: 13px; color: #888; }
  .container { display: flex; flex: 1; overflow: hidden; }
  .sidebar {
    width: 320px; min-width: 260px; background: #16213e;
    border-right: 1px solid #0f3460; overflow-y: auto; flex-shrink: 0;
  }
  .sidebar-header {
    padding: 12px 16px; font-size: 13px; color: #888;
    border-bottom: 1px solid #0f3460; position: sticky; top: 0;
    background: #16213e; z-index: 1;
  }
  .search-box {
    padding: 8px 16px; border-bottom: 1px solid #0f3460;
    position: sticky; top: 38px; background: #16213e; z-index: 1;
  }
  .search-box input {
    width: 100%; padding: 6px 10px; background: #1a1a2e; color: #e0e0e0;
    border: 1px solid #0f3460; border-radius: 4px; font-size: 13px;
    font-family: inherit; outline: none;
  }
  .search-box input:focus { border-color: #e94560; }
  .search-box input::placeholder { color: #555; }
  .sha-match { color: #e94560; font-size: 10px; font-family: monospace; }
  .clear-btn {
    background: none; border: none; color: #888; cursor: pointer;
    font-size: 12px; padding: 2px 6px; margin-left: 6px;
  }
  .clear-btn:hover { color: #e94560; }
  .exp-item {
    padding: 10px 16px; cursor: pointer; border-bottom: 1px solid #0f346033;
    transition: background 0.15s;
  }
  .exp-item:hover { background: #1a1a3e; }
  .exp-item.active { background: #0f3460; }
  .exp-item .label { font-size: 14px; font-weight: 500; color: #e0e0e0; }
  .exp-item .intent {
    font-size: 12px; color: #888; margin-top: 3px;
    white-space: nowrap; overflow: hidden; text-overflow: ellipsis;
  }
  .exp-item .meta {
    font-size: 11px; color: #555; margin-top: 4px;
    display: flex; gap: 10px;
  }
  .exp-item .meta .reverted { color: #e94560; }
  .exp-item .commit-id { font-size: 10px; color: #666; font-family: monospace; margin-top: 2px; }
  .main { flex: 1; display: flex; flex-direction: column; overflow: hidden; }
  .nav {
    padding: 10px 20px; background: #16213e;
    border-bottom: 1px solid #0f3460;
    display: flex; align-items: center; gap: 12px;
  }
  .nav button {
    background: #0f3460; color: #e0e0e0; border: 1px solid #1a3a6e;
    padding: 5px 14px; border-radius: 4px; cursor: pointer; font-size: 13px;
  }
  .nav button:hover { background: #1a3a6e; }
  .nav button:disabled { opacity: 0.3; cursor: default; }
  .nav .info { font-size: 13px; color: #888; }
  .tab-bar {
    padding: 0 20px; background: #16213e;
    border-bottom: 1px solid #0f3460;
    display: flex; gap: 0;
  }
  .tab {
    padding: 8px 16px; font-size: 13px; color: #888; cursor: pointer;
    border-bottom: 2px solid transparent; transition: all 0.15s;
  }
  .tab:hover { color: #e0e0e0; }
  .tab.active { color: #e94560; border-bottom-color: #e94560; }
  .viewer {
    flex: 1; overflow-y: auto; padding: 20px;
  }
  .viewer pre {
    white-space: pre-wrap; word-wrap: break-word; font-size: 13px;
    line-height: 1.6; color: #d0d0d0;
  }
  .empty {
    display: flex; align-items: center; justify-content: center;
    height: 100%; color: #555; font-size: 15px;
  }
  .loading { color: #888; padding: 20px; font-size: 13px; }
  .msg-header {
    text-align: center; color: #555; font-size: 12px; padding: 16px 0 8px;
    letter-spacing: 1px; text-transform: uppercase;
  }
  .msg-meta {
    font-size: 11px; color: #555; padding: 0 16px 12px; font-family: monospace;
  }
  .msg-user {
    background: #182840; border-left: 3px solid #4a9eff; padding: 10px 16px;
    margin: 6px 0; border-radius: 0 4px 4px 0;
  }
  .msg-user pre, .msg-assistant pre {
    white-space: pre-wrap; word-wrap: break-word; font-size: 13px;
    line-height: 1.6; color: #d0d0d0; margin: 4px 0 0; font-family: inherit;
  }
  .msg-assistant {
    background: #182e28; border-left: 3px solid #50c878; padding: 10px 16px;
    margin: 6px 0; border-radius: 0 4px 4px 0;
  }
  .msg-tool {
    background: #24242e; border-left: 3px solid #e9a045; margin: 6px 0;
    border-radius: 0 4px 4px 0;
  }
  .tool-toggle {
    padding: 8px 16px; cursor: pointer; user-select: none;
    display: flex; align-items: center; gap: 8px;
  }
  .tool-toggle:hover { background: #2e2e3a; }
  .toggle-icon { font-size: 10px; color: #e9a045; width: 14px; display: inline-block; }
  .tool-body { padding: 0 16px 10px; border-top: 1px solid #333; }
  .tool-body pre {
    white-space: pre-wrap; word-wrap: break-word; font-size: 12px;
    line-height: 1.5; color: #aaa; margin: 8px 0; font-family: monospace;
  }
  .msg-result {
    background: #1c1c26; padding: 8px 12px; border-radius: 4px; margin-top: 6px;
  }
  .msg-result pre {
    white-space: pre-wrap; word-wrap: break-word; font-size: 12px;
    line-height: 1.5; color: #999; margin: 4px 0 0; font-family: monospace;
  }
  .role-label {
    font-size: 10px; font-weight: 700; text-transform: uppercase;
    letter-spacing: 0.5px; margin-bottom: 2px; display: block;
  }
  .msg-user .role-label { color: #4a9eff; }
  .msg-assistant .role-label { color: #50c878; }
  .msg-tool .role-label { color: #e9a045; font-size: 12px; }
  .msg-result .role-label { color: #777; font-size: 10px; }
  .diff-header { color: #888; font-weight: bold; display: block; }
  .diff-old { background: #3c1f1f; color: #e8a0a0; display: block; margin: 0; padding: 1px 6px; }
  .diff-new { background: #1f3c1f; color: #a0e8a0; display: block; margin: 0; padding: 1px 6px; }
  .git-context {
    background: #12122a; border: 1px solid #0f3460; border-radius: 6px;
    padding: 16px; margin-bottom: 16px;
  }
  .git-context h3 { font-size: 13px; color: #e94560; margin-bottom: 8px; }
  .git-context .commit-info { font-size: 12px; color: #888; font-family: monospace; margin: 4px 0; }
  .git-context .files-list { font-size: 12px; color: #aaa; margin: 8px 0; }
  .git-context .files-list span { display: block; padding: 1px 0; }
  .git-context .diff-view {
    background: #0d0d1a; border: 1px solid #1a1a3e; border-radius: 4px;
    padding: 8px; margin-top: 8px; max-height: 300px; overflow-y: auto;
  }
  .git-context .diff-view pre {
    font-size: 11px; line-height: 1.4; color: #aaa; font-family: monospace;
    white-space: pre-wrap; word-wrap: break-word;
  }
</style>
</head>
<body>
<header>
  <h1>Experience Agent</h1>
  <span id="status">Loading...</span>
</header>
<div class="container">
  <div class="sidebar">
    <div class="sidebar-header">Experiences <span id="exp-count"></span></div>
    <div class="search-box">
      <input type="text" id="sha-search" placeholder="Filter by SHA (commit, head)..."
        oninput="onSearchInput(this.value)">
    </div>
    <div id="exp-list"></div>
  </div>
  <div class="main">
    <div class="tab-bar" id="tab-bar" style="display:none">
      <div class="tab active" onclick="switchTab('conversation')">Conversation</div>
      <div class="tab" onclick="switchTab('git')">Git Context</div>
    </div>
    <div class="nav" id="nav" style="display:none">
      <button id="btn-prev" onclick="prevInteraction()">Prev</button>
      <button id="btn-next" onclick="nextInteraction()">Next</button>
      <span class="info" id="nav-info"></span>
    </div>
    <div class="viewer" id="viewer">
      <div class="empty">Select an experience to browse</div>
    </div>
  </div>
</div>
<script>
let experiences = [];
let filteredExperiences = [];
let currentExp = null;
let currentIdx = 0;
let totalInteractions = 0;
let searchQuery = '';
let currentTab = 'conversation';

async function fetchJSON(url) {
  const r = await fetch(url);
  return r.json();
}

function escapeHtml(s) {
  const d = document.createElement('div');
  d.textContent = s;
  return d.innerHTML;
}

function isMarker(line) {
  return /^=== Interaction \d+ ===$/.test(line)
    || line.startsWith('User: ')
    || /^Branch: .* \| Time: /.test(line)
    || line.startsWith('Assistant: ')
    || /^\[Tool: .+\]$/.test(line)
    || line.startsWith('[Result]: ');
}

function formatDiff(text) {
  if (!text) return '';
  return text.split('\n').map(line => {
    if (line.startsWith('+++') || line.startsWith('---'))
      return '<span class="diff-header">' + escapeHtml(line) + '</span>';
    if (line.startsWith('+'))
      return '<span class="diff-new">' + escapeHtml(line) + '</span>';
    if (line.startsWith('-'))
      return '<span class="diff-old">' + escapeHtml(line) + '</span>';
    return escapeHtml(line) + '\n';
  }).join('');
}

function formatConversation(text) {
  var lines = text.split('\n');
  var html = '';
  var i = 0;
  while (i < lines.length) {
    var line = lines[i];
    if (/^=== Interaction \d+ ===$/.test(line)) {
      html += '<div class="msg-header">' + escapeHtml(line) + '</div>';
      i++;
    } else if (line.startsWith('User: ')) {
      var blk = line.substring(6);
      i++;
      while (i < lines.length && !isMarker(lines[i])) { blk += '\n' + lines[i]; i++; }
      html += '<div class="msg-user"><span class="role-label">User</span><pre>'
            + escapeHtml(blk.replace(/\n+$/, '')) + '</pre></div>';
    } else if (/^Branch: .* \| Time: /.test(line)) {
      html += '<div class="msg-meta">' + escapeHtml(line) + '</div>';
      i++;
    } else if (line.startsWith('Assistant: ')) {
      var blk = line.substring(11);
      i++;
      while (i < lines.length && !isMarker(lines[i])) { blk += '\n' + lines[i]; i++; }
      html += '<div class="msg-assistant"><span class="role-label">Assistant</span><pre>'
            + escapeHtml(blk.replace(/\n+$/, '')) + '</pre></div>';
    } else if (/^\[Tool: .+\]$/.test(line)) {
      var name = line.match(/^\[Tool: (.+)\]$/)[1];
      var detail = '';
      i++;
      while (i < lines.length && !isMarker(lines[i])) { detail += lines[i] + '\n'; i++; }
      var result = '';
      if (i < lines.length && lines[i].startsWith('[Result]: ')) {
        result = lines[i].substring(10);
        i++;
        while (i < lines.length && !isMarker(lines[i])) { result += '\n' + lines[i]; i++; }
      }
      var tid = 'tool-' + Math.random().toString(36).substr(2, 9);
      html += '<div class="msg-tool">'
        + '<div class="tool-toggle" onclick="toggleTool(\'' + tid + '\')">'
        + '<span class="toggle-icon" id="icon-' + tid + '">&#9654;</span>'
        + '<span class="role-label">Tool: ' + escapeHtml(name) + '</span></div>'
        + '<div class="tool-body" id="' + tid + '" style="display:none">';
      if (detail.trim()) html += '<pre>' + formatToolDetail(detail.replace(/\n+$/, '')) + '</pre>';
      if (result.trim()) {
        html += '<div class="msg-result"><span class="role-label">Result</span><pre>'
              + escapeHtml(result.replace(/\n+$/, '')) + '</pre></div>';
      }
      html += '</div></div>';
    } else if (line.startsWith('[Result]: ')) {
      var blk = line.substring(10);
      i++;
      while (i < lines.length && !isMarker(lines[i])) { blk += '\n' + lines[i]; i++; }
      html += '<div class="msg-result"><span class="role-label">Result</span><pre>'
            + escapeHtml(blk.replace(/\n+$/, '')) + '</pre></div>';
    } else {
      i++;
    }
  }
  return html;
}

function formatToolDetail(text) {
  var lines = text.split('\n');
  var html = '';
  var mode = 'normal';
  for (var j = 0; j < lines.length; j++) {
    var l = lines[j];
    if (l === '--- old') {
      mode = 'old';
      html += '<span class="diff-header">--- old</span>';
    } else if (l === '+++ new') {
      mode = 'new';
      html += '<span class="diff-header">+++ new</span>';
    } else if (mode === 'old') {
      html += '<span class="diff-old">- ' + escapeHtml(l) + '</span>';
    } else if (mode === 'new') {
      html += '<span class="diff-new">+ ' + escapeHtml(l) + '</span>';
    } else {
      html += escapeHtml(l) + '\n';
    }
  }
  return html;
}

function toggleTool(id) {
  var body = document.getElementById(id);
  var icon = document.getElementById('icon-' + id);
  if (body.style.display === 'none') {
    body.style.display = 'block';
    icon.innerHTML = '&#9660;';
  } else {
    body.style.display = 'none';
    icon.innerHTML = '&#9654;';
  }
}

function formatTime(ts) {
  if (!ts) return '';
  const d = new Date(ts * 1000);
  return d.toLocaleDateString() + ' ' + d.toLocaleTimeString([], {hour:'2-digit',minute:'2-digit'});
}

function shaMatches(exp, q) {
  if (!q) return true;
  q = q.toLowerCase();
  return (exp.id && exp.id.toLowerCase().startsWith(q))
      || (exp.commit_sha && exp.commit_sha.toLowerCase().startsWith(q))
      || (exp.head_sha && exp.head_sha.toLowerCase().startsWith(q));
}

function matchedShaField(exp, q) {
  if (!q) return '';
  q = q.toLowerCase();
  if (exp.id && exp.id.toLowerCase().startsWith(q))
    return 'id: ' + exp.id.substring(0, 10);
  if (exp.commit_sha && exp.commit_sha.toLowerCase().startsWith(q))
    return 'commit: ' + exp.commit_sha.substring(0, 10);
  if (exp.head_sha && exp.head_sha.toLowerCase().startsWith(q))
    return 'head: ' + exp.head_sha.substring(0, 10);
  return '';
}

function applyFilter() {
  if (!searchQuery) {
    filteredExperiences = experiences;
  } else {
    filteredExperiences = experiences.filter(exp => shaMatches(exp, searchQuery));
  }
  document.getElementById('exp-count').textContent = searchQuery
    ? '(' + filteredExperiences.length + '/' + experiences.length + ')'
    : '(' + experiences.length + ')';
}

function onSearchInput(val) {
  searchQuery = val.trim();
  applyFilter();
  renderList();
}

async function loadExperiences() {
  try {
    const data = await fetchJSON('/api/experiences');
    experiences = data.experiences || [];
    applyFilter();
    document.getElementById('status').textContent = experiences.length + ' experiences';
    renderList();
    const params = new URLSearchParams(window.location.search);
    const sha = params.get('sha');
    if (sha) {
      document.getElementById('sha-search').value = sha;
      onSearchInput(sha);
      if (filteredExperiences.length > 0) selectExperience(filteredExperiences[0]);
    }
  } catch(e) {
    document.getElementById('status').textContent = 'Error: ' + e.message;
  }
}

function renderList() {
  const el = document.getElementById('exp-list');
  el.innerHTML = '';
  filteredExperiences.forEach((exp, i) => {
    const div = document.createElement('div');
    div.className = 'exp-item' + (currentExp && currentExp.id === exp.id ? ' active' : '');
    const shaInfo = matchedShaField(exp, searchQuery);
    const commitId = exp.id ? exp.id.substring(0, 8) : '';
    div.innerHTML = '<div class="label">' + escapeHtml(exp.label) + '</div>'
      + '<div class="intent">' + escapeHtml(exp.intent) + '</div>'
      + (commitId ? '<div class="commit-id">' + escapeHtml(commitId) + '</div>' : '')
      + (shaInfo ? '<div class="sha-match">' + escapeHtml(shaInfo) + '</div>' : '')
      + '<div class="meta">'
      + '<span>' + formatTime(exp.timestamp) + '</span>'
      + '<span>' + escapeHtml(exp.branch || '') + '</span>'
      + (exp.reverted ? '<span class="reverted">reverted</span>' : '')
      + '</div>';
    div.onclick = () => selectExperience(exp);
    el.appendChild(div);
  });
}

function switchTab(tab) {
  currentTab = tab;
  document.querySelectorAll('.tab').forEach(t => t.classList.remove('active'));
  event.target.classList.add('active');
  if (tab === 'git') showGitContext();
  else if (totalInteractions > 0) loadInteraction(currentIdx);
}

function showGitContext() {
  if (!currentExp) return;
  var html = '<div class="git-context">';
  html += '<h3>Git Context</h3>';
  html += '<div class="commit-info">Commit: ' + escapeHtml(currentExp.id || '') + '</div>';
  html += '<div class="commit-info">Parent: ' + escapeHtml(currentExp.parent_sha || '') + '</div>';
  html += '<div class="commit-info">Branch: ' + escapeHtml(currentExp.branch || '') + '</div>';
  if (currentExp.commit_message) {
    html += '<div class="commit-info" style="margin-top:8px;color:#d0d0d0;white-space:pre-wrap">'
      + escapeHtml(currentExp.commit_message) + '</div>';
  }
  if (currentExp.files_changed && currentExp.files_changed.length > 0) {
    html += '<div class="files-list"><strong style="color:#e9a045">Changed files:</strong>';
    currentExp.files_changed.forEach(f => {
      html += '<span>' + escapeHtml(f) + '</span>';
    });
    html += '</div>';
  }
  if (currentExp.diff) {
    html += '<div class="diff-view"><pre>' + formatDiff(currentExp.diff) + '</pre></div>';
  }
  html += '</div>';
  document.getElementById('viewer').innerHTML = html;
}

async function selectExperience(exp) {
  currentExp = exp;
  currentIdx = 0;
  currentTab = 'conversation';
  renderList();
  document.getElementById('tab-bar').style.display = 'flex';
  document.querySelectorAll('.tab').forEach((t, i) => {
    t.classList.toggle('active', i === 0);
  });
  document.getElementById('viewer').innerHTML = '<div class="loading">Loading...</div>';
  try {
    const data = await fetchJSON('/api/experiences/' + encodeURIComponent(exp.id));
    // Update currentExp with full data including git context
    if (data.experience) {
      currentExp = data.experience;
    }
    totalInteractions = data.interactions_count || 0;
    if (totalInteractions > 0) {
      document.getElementById('nav').style.display = 'flex';
      loadInteraction(0);
    } else {
      document.getElementById('nav').style.display = 'none';
      if (currentExp.diff || currentExp.commit_message) {
        switchTab('git');
        document.querySelectorAll('.tab').forEach((t, i) => {
          t.classList.toggle('active', i === 1);
        });
      } else {
        document.getElementById('viewer').innerHTML = '<div class="empty">No interactions found</div>';
      }
    }
  } catch(e) {
    document.getElementById('viewer').innerHTML = '<div class="empty">Error: ' + escapeHtml(e.message) + '</div>';
  }
}

async function loadInteraction(idx) {
  currentIdx = idx;
  updateNav();
  document.getElementById('viewer').innerHTML = '<div class="loading">Loading interaction ' + idx + '...</div>';
  try {
    const data = await fetchJSON('/api/interactions/' + encodeURIComponent(currentExp.id) + '/' + idx);
    document.getElementById('viewer').innerHTML = formatConversation(data.formatted);
  } catch(e) {
    document.getElementById('viewer').innerHTML = '<div class="empty">Error: ' + escapeHtml(e.message) + '</div>';
  }
}

function updateNav() {
  document.getElementById('btn-prev').disabled = currentIdx <= 0;
  document.getElementById('btn-next').disabled = currentIdx >= totalInteractions - 1;
  document.getElementById('nav-info').textContent =
    'Interaction ' + (currentIdx + 1) + ' of ' + totalInteractions
    + (currentExp ? ' - ' + currentExp.label : '');
}

function prevInteraction() { if (currentIdx > 0) loadInteraction(currentIdx - 1); }
function nextInteraction() { if (currentIdx < totalInteractions - 1) loadInteraction(currentIdx + 1); }

document.addEventListener('keydown', e => {
  if (e.key === 'ArrowLeft') prevInteraction();
  if (e.key === 'ArrowRight') nextInteraction();
});

loadExperiences();
</script>
</body>
</html>|}

let blame_html = {|<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>Blame — Experience Agent</title>
<style>
  * { margin: 0; padding: 0; box-sizing: border-box; }
  body {
    font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, monospace;
    background: #1a1a2e; color: #e0e0e0; height: 100vh; display: flex;
    flex-direction: column;
  }
  header {
    background: #16213e; padding: 12px 20px; border-bottom: 1px solid #0f3460;
    display: flex; align-items: center; gap: 12px;
  }
  header h1 { font-size: 16px; color: #e94560; }
  header a { color: #888; font-size: 13px; text-decoration: none; }
  header a:hover { color: #e0e0e0; }
  .controls {
    padding: 12px 20px; background: #16213e; border-bottom: 1px solid #0f3460;
    display: flex; gap: 10px; align-items: center; flex-wrap: wrap;
  }
  .controls input {
    padding: 6px 10px; background: #1a1a2e; color: #e0e0e0;
    border: 1px solid #0f3460; border-radius: 4px; font-size: 13px;
    font-family: inherit; outline: none;
  }
  .controls input:focus { border-color: #e94560; }
  .controls input::placeholder { color: #555; }
  .controls input.filepath { width: 320px; }
  .controls input.small { width: 80px; }
  .controls button {
    background: #0f3460; color: #e0e0e0; border: 1px solid #1a3a6e;
    padding: 6px 16px; border-radius: 4px; cursor: pointer; font-size: 13px;
  }
  .controls button:hover { background: #1a3a6e; }
  .controls label { font-size: 12px; color: #888; }
  .stats {
    padding: 8px 20px; font-size: 12px; color: #888;
    border-bottom: 1px solid #0f346033;
  }
  .split { display: flex; flex: 1; overflow: hidden; }
  .blame-pane { flex: 1; overflow: auto; }
  .conv-pane {
    width: 0; overflow-y: auto; background: #16213e;
    border-left: 1px solid #0f3460; transition: width 0.2s;
  }
  .conv-pane.open { width: 50%; min-width: 400px; }
  .conv-header {
    padding: 10px 16px; border-bottom: 1px solid #0f3460;
    display: flex; align-items: center; justify-content: space-between;
    position: sticky; top: 0; background: #16213e; z-index: 2;
  }
  .conv-header h3 { font-size: 13px; color: #e94560; }
  .conv-header .close-btn {
    background: none; border: none; color: #888; cursor: pointer;
    font-size: 18px; line-height: 1;
  }
  .conv-header .close-btn:hover { color: #e94560; }
  .conv-nav {
    padding: 8px 16px; border-bottom: 1px solid #0f3460;
    display: flex; align-items: center; gap: 8px;
  }
  .conv-nav button {
    background: #0f3460; color: #e0e0e0; border: 1px solid #1a3a6e;
    padding: 4px 10px; border-radius: 3px; cursor: pointer; font-size: 12px;
  }
  .conv-nav button:hover { background: #1a3a6e; }
  .conv-nav button:disabled { opacity: 0.3; cursor: default; }
  .conv-nav span { font-size: 12px; color: #888; }
  .conv-body { padding: 16px; }
  .conv-summary {
    background: #12122a; border: 1px solid #0f3460; border-radius: 6px;
    padding: 12px 16px; margin-bottom: 16px;
  }
  .conv-summary .label { font-size: 14px; color: #e94560; font-weight: 500; }
  .conv-summary .intent { font-size: 12px; color: #aaa; margin-top: 4px; }
  .conv-summary .summary { font-size: 12px; color: #d0d0d0; margin-top: 8px; line-height: 1.5; }
  .conv-summary .sha { font-size: 10px; color: #555; font-family: monospace; margin-top: 6px; }
  table { width: 100%; border-collapse: collapse; font-size: 13px; }
  tr { border-bottom: 1px solid #0f346022; }
  tr:hover { background: #1a1a3e; }
  tr.group-start { border-top: 1px solid #0f346066; }
  tr.active-exp { background: #0f346044; }
  td { padding: 1px 8px; vertical-align: top; white-space: nowrap; }
  .col-line { color: #555; text-align: right; width: 50px; user-select: none; }
  .col-sha {
    font-family: monospace; font-size: 11px; width: 72px; color: #888;
    cursor: pointer;
  }
  .col-sha:hover { color: #e94560; }
  .col-code { white-space: pre; font-family: monospace; color: #d0d0d0; width: 100%; }
  .col-exp {
    font-size: 11px; color: #50c878; max-width: 240px;
    overflow: hidden; text-overflow: ellipsis; cursor: pointer;
  }
  .col-exp:hover { color: #a0f0c0; }
  .empty { display: flex; align-items: center; justify-content: center;
    height: 100%; color: #555; font-size: 15px; }
  .loading { color: #888; padding: 20px; font-size: 13px; }
  /* Conversation message styles */
  .msg-header {
    text-align: center; color: #555; font-size: 12px; padding: 16px 0 8px;
    letter-spacing: 1px; text-transform: uppercase;
  }
  .msg-meta {
    font-size: 11px; color: #555; padding: 0 0 12px; font-family: monospace;
  }
  .msg-user {
    background: #182840; border-left: 3px solid #4a9eff; padding: 10px 16px;
    margin: 6px 0; border-radius: 0 4px 4px 0;
  }
  .msg-user pre, .msg-assistant pre, .msg-thinking pre {
    white-space: pre-wrap; word-wrap: break-word; font-size: 13px;
    line-height: 1.6; color: #d0d0d0; margin: 4px 0 0; font-family: inherit;
  }
  .msg-assistant {
    background: #182e28; border-left: 3px solid #50c878; padding: 10px 16px;
    margin: 6px 0; border-radius: 0 4px 4px 0;
  }
  .msg-thinking {
    background: #2a1e38; border-left: 3px solid #c084fc; padding: 10px 16px;
    margin: 6px 0; border-radius: 0 4px 4px 0;
  }
  .msg-thinking pre { color: #bba0d8; font-size: 12px; }
  .msg-tool {
    background: #24242e; border-left: 3px solid #e9a045; margin: 6px 0;
    border-radius: 0 4px 4px 0;
  }
  .tool-toggle {
    padding: 8px 16px; cursor: pointer; user-select: none;
    display: flex; align-items: center; gap: 8px;
  }
  .tool-toggle:hover { background: #2e2e3a; }
  .toggle-icon { font-size: 10px; color: #e9a045; width: 14px; display: inline-block; }
  .tool-body { padding: 0 16px 10px; border-top: 1px solid #333; }
  .tool-body pre {
    white-space: pre-wrap; word-wrap: break-word; font-size: 12px;
    line-height: 1.5; color: #aaa; margin: 8px 0; font-family: monospace;
  }
  .msg-result {
    background: #1c1c26; padding: 8px 12px; border-radius: 4px; margin-top: 6px;
  }
  .msg-result pre {
    white-space: pre-wrap; word-wrap: break-word; font-size: 12px;
    line-height: 1.5; color: #999; margin: 4px 0 0; font-family: monospace;
  }
  .role-label {
    font-size: 10px; font-weight: 700; text-transform: uppercase;
    letter-spacing: 0.5px; margin-bottom: 2px; display: block;
  }
  .msg-user .role-label { color: #4a9eff; }
  .msg-assistant .role-label { color: #50c878; }
  .msg-thinking .role-label { color: #c084fc; }
  .msg-tool .role-label { color: #e9a045; font-size: 12px; }
  .msg-result .role-label { color: #777; font-size: 10px; }
  .diff-header { color: #888; font-weight: bold; display: block; }
  .diff-old { background: #3c1f1f; color: #e8a0a0; display: block; margin: 0; padding: 1px 6px; }
  .diff-new { background: #1f3c1f; color: #a0e8a0; display: block; margin: 0; padding: 1px 6px; }
</style>
</head>
<body>
<header>
  <h1>Blame</h1>
  <a href="/">← Experiences</a>
</header>
<div class="controls">
  <label>File:</label>
  <input type="text" class="filepath" id="filepath" placeholder="lib/chromadb.ml">
  <label>Lines:</label>
  <input type="number" class="small" id="line-start" placeholder="from">
  <input type="number" class="small" id="line-end" placeholder="to">
  <label>Pattern:</label>
  <input type="text" id="pattern" placeholder="filter lines...">
  <button onclick="runBlame()">Blame</button>
</div>
<div class="stats" id="stats" style="display:none"></div>
<div class="split">
  <div class="blame-pane" id="viewer">
    <div class="empty">Enter a file path and click Blame</div>
  </div>
  <div class="conv-pane" id="conv-pane">
    <div class="conv-header">
      <h3 id="conv-title">Conversation</h3>
      <button class="close-btn" onclick="closeConv()">&times;</button>
    </div>
    <div class="conv-nav" id="conv-nav" style="display:none">
      <button id="btn-prev-int" onclick="prevInt()">Prev</button>
      <button id="btn-next-int" onclick="nextInt()">Next</button>
      <span id="conv-nav-info"></span>
    </div>
    <div class="conv-body" id="conv-body">
      <div class="empty">Click an experience label to view the conversation</div>
    </div>
  </div>
</div>
<script>
const shaColors = {};
const palette = [
  '#e94560','#4a9eff','#50c878','#e9a045','#c084fc',
  '#f472b6','#38bdf8','#a3e635','#fb923c','#94a3b8',
];
let colorIdx = 0;
let blameData = null;
let activeExpId = null;
let convInteractions = 0;
let convIdx = 0;

function shaColor(sha) {
  if (!shaColors[sha]) {
    shaColors[sha] = palette[colorIdx % palette.length];
    colorIdx++;
  }
  return shaColors[sha];
}

function escapeHtml(s) {
  const d = document.createElement('div');
  d.textContent = s;
  return d.innerHTML;
}

function initFromURL() {
  const params = new URLSearchParams(window.location.search);
  const file = params.get('file');
  if (file) {
    document.getElementById('filepath').value = file;
    const ls = params.get('line_start');
    const le = params.get('line_end');
    const pat = params.get('pattern');
    if (ls) document.getElementById('line-start').value = ls;
    if (le) document.getElementById('line-end').value = le;
    if (pat) document.getElementById('pattern').value = pat;
    runBlame();
  }
}

async function runBlame() {
  const filepath = document.getElementById('filepath').value.trim();
  if (!filepath) return;
  const lineStart = document.getElementById('line-start').value;
  const lineEnd = document.getElementById('line-end').value;
  const pattern = document.getElementById('pattern').value.trim();

  let url = '/api/blame?filepath=' + encodeURIComponent(filepath);
  if (lineStart) url += '&line_start=' + lineStart;
  if (lineEnd) url += '&line_end=' + lineEnd;
  if (pattern) url += '&pattern=' + encodeURIComponent(pattern);

  document.getElementById('viewer').innerHTML = '<div class="loading">Running blame...</div>';
  document.getElementById('stats').style.display = 'none';
  closeConv();

  try {
    const resp = await fetch(url);
    const data = await resp.json();
    if (data.error) {
      document.getElementById('viewer').innerHTML =
        '<div class="empty">' + escapeHtml(data.error) + '</div>';
      return;
    }
    blameData = data;
    renderBlame(data);
  } catch(e) {
    document.getElementById('viewer').innerHTML =
      '<div class="empty">Error: ' + escapeHtml(e.message) + '</div>';
  }
}

function renderBlame(data) {
  const lines = data.lines || [];
  const stats = document.getElementById('stats');
  stats.style.display = 'block';
  stats.textContent = data.total_lines + ' lines, '
    + data.unique_commits + ' commits, '
    + data.experiences_found + ' experiences found';

  if (lines.length === 0) {
    document.getElementById('viewer').innerHTML = '<div class="empty">No lines to show</div>';
    return;
  }

  let html = '<table>';
  let prevSha = null;
  for (const line of lines) {
    const isNewGroup = line.full_sha !== prevSha;
    const cls = isNewGroup ? 'group-start' : '';
    const expCls = (activeExpId && line.experience_id === activeExpId) ? ' active-exp' : '';
    const color = shaColor(line.sha);
    const hasExp = !!line.experience_id;
    const shaCell = isNewGroup ? escapeHtml(line.sha) : '';
    const expLabel = (isNewGroup && hasExp) ? escapeHtml(line.experience_label || '') : '';

    html += '<tr class="' + cls + expCls + '">'
      + '<td class="col-line">' + line.line + '</td>'
      + '<td class="col-sha" style="color:' + color + '">' + shaCell + '</td>'
      + '<td class="col-exp"'
      + (hasExp ? ' onclick="openExp(\'' + escapeHtml(line.experience_id) + '\',\''
        + escapeHtml(line.experience_label || '') + '\',\''
        + escapeHtml(line.full_sha) + '\')"' : '')
      + '>' + expLabel + '</td>'
      + '<td class="col-code">' + escapeHtml(line.content) + '</td>'
      + '</tr>';
    prevSha = line.full_sha;
  }
  html += '</table>';
  document.getElementById('viewer').innerHTML = html;
}

function closeConv() {
  document.getElementById('conv-pane').classList.remove('open');
  activeExpId = null;
  if (blameData) renderBlame(blameData);
}

async function openExp(expId, label, sha) {
  activeExpId = expId;
  if (blameData) renderBlame(blameData);
  const pane = document.getElementById('conv-pane');
  pane.classList.add('open');
  document.getElementById('conv-title').textContent = label || 'Conversation';
  document.getElementById('conv-body').innerHTML = '<div class="loading">Loading conversation...</div>';
  document.getElementById('conv-nav').style.display = 'none';

  try {
    const data = await fetch('/api/experiences/' + encodeURIComponent(expId)).then(r => r.json());
    if (!data.experience) {
      document.getElementById('conv-body').innerHTML =
        '<div class="empty">Experience not found</div>';
      return;
    }
    const exp = data.experience;
    convInteractions = data.interactions_count || 0;
    convIdx = 0;

    // Show summary header
    let summaryHtml = '<div class="conv-summary">'
      + '<div class="label">' + escapeHtml(exp.label) + '</div>'
      + '<div class="intent">' + escapeHtml(exp.intent) + '</div>'
      + '<div class="sha">' + escapeHtml(exp.id) + '</div>'
      + '</div>';

    if (convInteractions > 0) {
      document.getElementById('conv-nav').style.display = 'flex';
      document.getElementById('conv-body').innerHTML = summaryHtml
        + '<div id="conv-content"><div class="loading">Loading interaction...</div></div>';
      loadInteraction(0);
    } else {
      document.getElementById('conv-body').innerHTML = summaryHtml
        + (exp.diff
          ? '<div style="margin-top:12px"><pre style="font-size:12px;color:#aaa;white-space:pre-wrap">'
            + formatDiff(exp.diff) + '</pre></div>'
          : '<div class="empty" style="height:auto;padding:20px">No JSONL interactions available (commit-only experience)</div>');
    }
  } catch(e) {
    document.getElementById('conv-body').innerHTML =
      '<div class="empty">Error: ' + escapeHtml(e.message) + '</div>';
  }
}

async function loadInteraction(idx) {
  convIdx = idx;
  updateConvNav();
  const el = document.getElementById('conv-content');
  if (!el) return;
  el.innerHTML = '<div class="loading">Loading interaction ' + (idx + 1) + '...</div>';
  try {
    const data = await fetch('/api/interactions/' + encodeURIComponent(activeExpId) + '/' + idx)
      .then(r => r.json());
    el.innerHTML = formatConversation(data.formatted);
  } catch(e) {
    el.innerHTML = '<div class="empty">Error: ' + escapeHtml(e.message) + '</div>';
  }
}

function updateConvNav() {
  document.getElementById('btn-prev-int').disabled = convIdx <= 0;
  document.getElementById('btn-next-int').disabled = convIdx >= convInteractions - 1;
  document.getElementById('conv-nav-info').textContent =
    'Interaction ' + (convIdx + 1) + ' of ' + convInteractions;
}

function prevInt() { if (convIdx > 0) loadInteraction(convIdx - 1); }
function nextInt() { if (convIdx < convInteractions - 1) loadInteraction(convIdx + 1); }

function isMarker(line) {
  return /^=== Interaction \d+ ===$/.test(line)
    || line.startsWith('User: ')
    || /^Branch: .* \| Time: /.test(line)
    || line.startsWith('Assistant: ')
    || line.startsWith('[Thinking]: ')
    || /^\[Tool: .+\]$/.test(line)
    || line.startsWith('[Result]: ');
}

function formatDiff(text) {
  if (!text) return '';
  return text.split('\n').map(line => {
    if (line.startsWith('+++') || line.startsWith('---'))
      return '<span class="diff-header">' + escapeHtml(line) + '</span>';
    if (line.startsWith('+'))
      return '<span class="diff-new">' + escapeHtml(line) + '</span>';
    if (line.startsWith('-'))
      return '<span class="diff-old">' + escapeHtml(line) + '</span>';
    return escapeHtml(line) + '\n';
  }).join('');
}

function formatConversation(text) {
  var lines = text.split('\n');
  var html = '';
  var i = 0;
  while (i < lines.length) {
    var line = lines[i];
    if (/^=== Interaction \d+ ===$/.test(line)) {
      html += '<div class="msg-header">' + escapeHtml(line) + '</div>';
      i++;
    } else if (line.startsWith('User: ')) {
      var blk = line.substring(6);
      i++;
      while (i < lines.length && !isMarker(lines[i])) { blk += '\n' + lines[i]; i++; }
      html += '<div class="msg-user"><span class="role-label">User</span><pre>'
            + escapeHtml(blk.replace(/\n+$/, '')) + '</pre></div>';
    } else if (/^Branch: .* \| Time: /.test(line)) {
      html += '<div class="msg-meta">' + escapeHtml(line) + '</div>';
      i++;
    } else if (line.startsWith('[Thinking]: ')) {
      var blk = line.substring(12);
      i++;
      while (i < lines.length && !isMarker(lines[i])) { blk += '\n' + lines[i]; i++; }
      var tid = 'think-' + Math.random().toString(36).substr(2, 9);
      html += '<div class="msg-thinking">'
        + '<div class="tool-toggle" onclick="toggleTool(\'' + tid + '\')">'
        + '<span class="toggle-icon" id="icon-' + tid + '">&#9654;</span>'
        + '<span class="role-label">Thinking</span></div>'
        + '<div class="tool-body" id="' + tid + '" style="display:none">'
        + '<pre>' + escapeHtml(blk.replace(/\n+$/, '')) + '</pre>'
        + '</div></div>';
    } else if (line.startsWith('Assistant: ')) {
      var blk = line.substring(11);
      i++;
      while (i < lines.length && !isMarker(lines[i])) { blk += '\n' + lines[i]; i++; }
      html += '<div class="msg-assistant"><span class="role-label">Assistant</span><pre>'
            + escapeHtml(blk.replace(/\n+$/, '')) + '</pre></div>';
    } else if (/^\[Tool: .+\]$/.test(line)) {
      var name = line.match(/^\[Tool: (.+)\]$/)[1];
      var detail = '';
      i++;
      while (i < lines.length && !isMarker(lines[i])) { detail += lines[i] + '\n'; i++; }
      var result = '';
      if (i < lines.length && lines[i].startsWith('[Result]: ')) {
        result = lines[i].substring(10);
        i++;
        while (i < lines.length && !isMarker(lines[i])) { result += '\n' + lines[i]; i++; }
      }
      var tid = 'tool-' + Math.random().toString(36).substr(2, 9);
      html += '<div class="msg-tool">'
        + '<div class="tool-toggle" onclick="toggleTool(\'' + tid + '\')">'
        + '<span class="toggle-icon" id="icon-' + tid + '">&#9654;</span>'
        + '<span class="role-label">Tool: ' + escapeHtml(name) + '</span></div>'
        + '<div class="tool-body" id="' + tid + '" style="display:none">';
      if (detail.trim()) html += '<pre>' + escapeHtml(detail.replace(/\n+$/, '')) + '</pre>';
      if (result.trim()) {
        html += '<div class="msg-result"><span class="role-label">Result</span><pre>'
              + escapeHtml(result.replace(/\n+$/, '')) + '</pre></div>';
      }
      html += '</div></div>';
    } else if (line.startsWith('[Result]: ')) {
      var blk = line.substring(10);
      i++;
      while (i < lines.length && !isMarker(lines[i])) { blk += '\n' + lines[i]; i++; }
      html += '<div class="msg-result"><span class="role-label">Result</span><pre>'
            + escapeHtml(blk.replace(/\n+$/, '')) + '</pre></div>';
    } else {
      i++;
    }
  }
  return html;
}

function toggleTool(id) {
  var body = document.getElementById(id);
  var icon = document.getElementById('icon-' + id);
  if (body.style.display === 'none') {
    body.style.display = 'block';
    icon.innerHTML = '&#9660;';
  } else {
    body.style.display = 'none';
    icon.innerHTML = '&#9654;';
  }
}

document.getElementById('filepath').addEventListener('keydown', e => {
  if (e.key === 'Enter') runBlame();
});
document.getElementById('pattern').addEventListener('keydown', e => {
  if (e.key === 'Enter') runBlame();
});

initFromURL();
</script>
</body>
</html>|}

(* Helper: parse path segments *)
let path_segments path =
  String.split_on_char '/' path
  |> List.filter (fun s -> String.length s > 0)

(* JSON response helper *)
let json_response ?(status=`OK) body =
  let headers = Cohttp.Header.of_list [
    "Content-Type", "application/json";
    "Access-Control-Allow-Origin", "*";
  ] in
  Cohttp_lwt_unix.Server.respond_string ~status ~headers
    ~body:(Yojson.Safe.to_string body) ()

let html_response body =
  let headers = Cohttp.Header.of_list [
    "Content-Type", "text/html; charset=utf-8";
  ] in
  Cohttp_lwt_unix.Server.respond_string ~status:`OK ~headers ~body ()

let error_response status msg =
  json_response ~status (`Assoc ["error", `String msg])

(* API: GET /api/experiences[?sha=X] *)
let handle_list_experiences state ~sha_filter =
  let* collection_id = Mcp_server.get_collection state in
  let+ exps = Chromadb.list_all ~port:state.Mcp_server.port
    ~collection_id ~limit:100 in
  let filtered = match sha_filter with
    | None -> exps
    | Some sha -> List.filter (Mcp_server.sha_matches ~sha) exps
  in
  let sorted = List.sort (fun (a : experience) (b : experience) ->
    Float.compare b.timestamp a.timestamp
  ) filtered in
  `Assoc [
    "experiences", `List (List.map experience_to_yojson sorted);
  ]

(* API: GET /api/experiences/:id — metadata + interaction summaries + git context *)
let handle_get_experience state exp_id =
  let* collection_id = Mcp_server.get_collection state in
  let* exps = Chromadb.list_all ~port:state.Mcp_server.port
    ~collection_id ~limit:100 in
  match List.find_opt (fun (e : experience) -> e.id = exp_id) exps with
  | None -> Lwt.return None
  | Some exp ->
    let jsonl_path = Filename.concat state.Mcp_server.jsonl_dir
      (exp.session_id ^ ".jsonl") in
    if exp.session_id <> "" && Sys.file_exists jsonl_path then begin
      let interactions = Jsonl_reader.parse_interactions ~filepath:jsonl_path in
      let matching = List.filter (fun (i : interaction) ->
        List.mem i.user_uuid exp.interaction_uuids
      ) interactions in
      let summaries = List.map (fun (i : interaction) ->
        `Assoc [
          "index", `Int i.index;
          "user_text", `String (Jsonl_reader.truncate 120 i.user_text);
          "timestamp", `String i.timestamp;
        ]
      ) matching in
      Lwt.return (Some (`Assoc [
        "experience", experience_to_yojson exp;
        "interactions", `List summaries;
        "interactions_count", `Int (List.length matching);
      ]))
    end else
      Lwt.return (Some (`Assoc [
        "experience", experience_to_yojson exp;
        "interactions", `List [];
        "interactions_count", `Int 0;
      ]))

(* API: GET /api/interactions/:exp_id/:index *)
let handle_get_interaction state exp_id index =
  let* collection_id = Mcp_server.get_collection state in
  let* exps = Chromadb.list_all ~port:state.Mcp_server.port
    ~collection_id ~limit:100 in
  match List.find_opt (fun (e : experience) -> e.id = exp_id) exps with
  | None -> Lwt.return None
  | Some exp ->
    let jsonl_path = Filename.concat state.Mcp_server.jsonl_dir
      (exp.session_id ^ ".jsonl") in
    if exp.session_id <> "" && Sys.file_exists jsonl_path then begin
      let interactions = Jsonl_reader.parse_interactions ~filepath:jsonl_path in
      let matching = List.filter (fun (i : interaction) ->
        List.mem i.user_uuid exp.interaction_uuids
      ) interactions in
      let total = List.length matching in
      match List.nth_opt matching index with
      | None -> Lwt.return None
      | Some interaction ->
        let formatted = Jsonl_reader.format_interaction
          ~filepath:jsonl_path interaction in
        Lwt.return (Some (`Assoc [
          "formatted", `String formatted;
          "index", `Int index;
          "total", `Int total;
          "user_text", `String interaction.user_text;
          "timestamp", `String interaction.timestamp;
        ]))
    end else
      Lwt.return None

(* API: GET /api/blame?filepath=X&line_start=Y&line_end=Z&pattern=P *)
let handle_blame state uri =
  let filepath = Uri.get_query_param uri "filepath" in
  match filepath with
  | None -> Lwt.return (`Assoc ["error", `String "filepath parameter is required"])
  | Some filepath ->
    let line_start = Uri.get_query_param uri "line_start"
      |> Option.map int_of_string_opt |> Option.join in
    let line_end = Uri.get_query_param uri "line_end"
      |> Option.map int_of_string_opt |> Option.join in
    let pattern = Uri.get_query_param uri "pattern" in
    let line_range = match line_start, line_end with
      | Some s, Some e -> Some (s, e)
      | Some s, None -> Some (s, s)
      | None, Some e -> Some (1, e)
      | None, None -> None
    in
    Lwt.catch (fun () ->
      let* blame_lines = Git_ops.blame ~cwd:state.Mcp_server.project_dir
        ?line_range ~filepath () in
      let filtered = match pattern with
        | None -> blame_lines
        | Some pat ->
          let pat_lower = String.lowercase_ascii pat in
          List.filter (fun (_sha, _ln, content) ->
            let content_lower = String.lowercase_ascii content in
            let rec find pos =
              if pos + String.length pat_lower > String.length content_lower then false
              else if String.sub content_lower pos (String.length pat_lower) = pat_lower then true
              else find (pos + 1)
            in
            find 0
          ) blame_lines
      in
      let unique_shas = List.fold_left (fun acc (sha, _, _) ->
        if List.mem sha acc then acc else sha :: acc
      ) [] filtered |> List.rev in
      let* collection_id = Mcp_server.get_collection state in
      let* all_with_summaries = Chromadb.list_all_with_summaries ~port:state.Mcp_server.port
        ~collection_id ~limit:1000 in
      let sha_to_exp = List.filter_map (fun sha ->
        match List.find_opt (fun ((e : experience), _) ->
          Mcp_server.sha_matches ~sha e
        ) all_with_summaries with
        | Some (e, summary) -> Some (sha, (e, summary))
        | None -> None
      ) unique_shas in
      let lines_json = List.map (fun (sha, ln, content) ->
        let short_sha = String.sub sha 0 (min 8 (String.length sha)) in
        let exp_fields = match List.assoc_opt sha sha_to_exp with
          | Some ((e : experience), summary) ->
            ["experience_id", `String e.id;
             "experience_label", `String e.label;
             "experience_intent", `String e.intent;
             "experience_summary", `String summary]
          | None -> []
        in
        `Assoc ([
          "line", `Int ln;
          "content", `String content;
          "sha", `String short_sha;
          "full_sha", `String sha;
        ] @ exp_fields)
      ) filtered in
      let exp_count = List.length sha_to_exp in
      Lwt.return (`Assoc [
        "filepath", `String filepath;
        "total_lines", `Int (List.length filtered);
        "unique_commits", `Int (List.length unique_shas);
        "experiences_found", `Int exp_count;
        "lines", `List lines_json;
      ])
    ) (fun exn ->
      Lwt.return (`Assoc ["error", `String (Printexc.to_string exn)]))

(* Route dispatcher *)
let handle_request state _conn req _body =
  let uri = Cohttp.Request.uri req in
  let path = Uri.path uri in
  let meth = Cohttp.Request.meth req in
  match meth with
  | `GET -> begin
    match path_segments path with
    | [] | [""] ->
      html_response index_html
    | ["blame"] ->
      html_response blame_html
    | ["api"; "blame"] ->
      let* result = handle_blame state uri in
      json_response result
    | ["api"; "experiences"] ->
      let sha_filter = Uri.get_query_param uri "sha" in
      let* result = handle_list_experiences state ~sha_filter in
      json_response result
    | ["api"; "experiences"; id] ->
      let* result = handle_get_experience state id in
      (match result with
       | Some j -> json_response j
       | None -> error_response `Not_found "Experience not found")
    | ["api"; "interactions"; exp_id; idx_str] ->
      (match int_of_string_opt idx_str with
       | None -> error_response `Bad_request "Invalid index"
       | Some idx ->
         let* result = handle_get_interaction state exp_id idx in
         (match result with
          | Some j -> json_response j
          | None -> error_response `Not_found "Interaction not found"))
    | _ ->
      error_response `Not_found "Not found"
  end
  | _ ->
    error_response `Method_not_allowed "Method not allowed"

let start ~port ~state =
  let callback = handle_request state in
  let server = Cohttp_lwt_unix.Server.make ~callback () in
  let* () = Lwt_io.eprintf "Web UI available at http://localhost:%d\n" port in
  Cohttp_lwt_unix.Server.create
    ~mode:(`TCP (`Port port))
    server
