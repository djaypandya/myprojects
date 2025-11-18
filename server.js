const http = require('http');
const fs = require('fs');
const path = require('path');
const url = require('url');

const PORT = process.env.PORT || 3000;
const PUBLIC_DIR = path.join(__dirname, 'public');

let outcomeId = 1;
let stepId = 1;
let noteId = 1;
let routineId = 1;

const outcomes = [];
const steps = [];
const notes = [];
const routines = [];

function now() {
  return new Date().toISOString();
}

function sendJson(res, status, data) {
  res.writeHead(status, {
    'Content-Type': 'application/json',
    'Access-Control-Allow-Origin': '*',
    'Access-Control-Allow-Methods': 'GET,POST,PATCH,OPTIONS',
    'Access-Control-Allow-Headers': 'Content-Type',
  });
  res.end(JSON.stringify(data));
}

function parseBody(req) {
  return new Promise((resolve, reject) => {
    let data = '';
    req.on('data', (chunk) => (data += chunk));
    req.on('end', () => {
      if (!data) return resolve({});
      try {
        resolve(JSON.parse(data));
      } catch (err) {
        reject(err);
      }
    });
  });
}

function createOutcome(data) {
  const newOutcome = {
    id: outcomeId++,
    title: data.title || 'Untitled Outcome',
    description: data.description || '',
    status: data.status || 'active',
    horizon: data.horizon || 'someday',
    created_at: now(),
    updated_at: now(),
  };
  outcomes.push(newOutcome);
  return newOutcome;
}

function createStep(data) {
  if (data.planned_for === 'today' && data.status !== 'done') {
    const mitCount = steps.filter((s) => s.planned_for === 'today' && s.status === 'todo').length;
    if (mitCount >= 3) return null;
  }
  const newStep = {
    id: stepId++,
    outcome_id: data.outcome_id || null,
    title: data.title || 'Untitled Step',
    status: data.status || 'todo',
    planned_for: data.planned_for || null,
    time_block: data.time_block || null,
    has_lower_priority: data.has_lower_priority || false,
    created_at: now(),
    updated_at: now(),
  };
  steps.push(newStep);
  return newStep;
}

function updateStep(id, updates) {
  const step = steps.find((s) => s.id === id);
  if (!step) return null;
  if (updates.planned_for === 'today' && updates.status !== 'done') {
    const mitCount = steps.filter(
      (s) => s.planned_for === 'today' && s.status === 'todo' && s.id !== id
    ).length;
    if (mitCount >= 3) return step;
  }
  Object.assign(step, updates, { updated_at: now() });
  return step;
}

function updateOutcome(id, updates) {
  const outcome = outcomes.find((o) => o.id === id);
  if (!outcome) return null;
  if (updates.horizon === 'this_week') {
    const weeklyCount = outcomes.filter((o) => o.horizon === 'this_week' && o.id !== id).length;
    if (weeklyCount >= 3) return outcome;
  }
  Object.assign(outcome, updates, { updated_at: now() });
  return outcome;
}

function createNote(data) {
  const newNote = {
    id: noteId++,
    content: data.content || '',
    linked_outcome_id: data.linked_outcome_id || null,
    created_at: now(),
  };
  notes.push(newNote);
  return newNote;
}

function getTodayState() {
  return {
    today_steps: steps.filter((s) => s.planned_for === 'today' || s.planned_for === 'today_extra'),
    active_this_week_outcomes: outcomes.filter((o) => o.horizon === 'this_week'),
    current_step: steps.find((s) => s.status === 'doing') || null,
  };
}

function simpleActionPlan(message) {
  const actions = [];
  const lower = message.toLowerCase();

  if (lower.includes('done')) {
    const doing = steps.find((s) => s.status === 'doing');
    if (doing) actions.push({ type: 'update_step', id: doing.id, status: 'done' });
  }

  if (lower.includes('plan today') || lower.includes("let's plan today") || lower.includes('plan my day')) {
    const weekly = outcomes.filter((o) => o.horizon === 'this_week');
    if (weekly.length === 0) actions.push({ type: 'create_outcome', title: 'Define weekly outcome', horizon: 'this_week' });
    weekly.slice(0, 3).forEach((outcome) => {
      actions.push({
        type: 'create_step',
        title: `Progress on ${outcome.title}`,
        outcome_id: outcome.id,
        planned_for: 'today',
        time_block: 'morning',
      });
    });
  }

  if (lower.startsWith('new outcome:')) {
    const title = message.split(':')[1].trim();
    actions.push({ type: 'create_outcome', title, horizon: 'this_week' });
  }

  if (lower.startsWith('new step:')) {
    const title = message.split(':')[1].trim();
    actions.push({ type: 'create_step', title, planned_for: 'today' });
  }

  const reply = actions.length
    ? 'Got it. I updated your plan based on your message.'
    : "I'm here to help. Tell me what you want to work on today.";

  return { actions, reply };
}

function applyActions(actions) {
  const results = [];
  actions.forEach((action) => {
    switch (action.type) {
      case 'create_step': {
        const newStep = createStep(action);
        if (newStep) results.push({ type: 'create_step', step: newStep });
        break;
      }
      case 'update_step': {
        const updated = updateStep(action.id, action);
        if (updated) results.push({ type: 'update_step', step: updated });
        break;
      }
      case 'create_outcome': {
        const weeklyCount = outcomes.filter((o) => o.horizon === 'this_week').length;
        if (action.horizon === 'this_week' && weeklyCount >= 3) break;
        const newOutcome = createOutcome(action);
        results.push({ type: 'create_outcome', outcome: newOutcome });
        break;
      }
      case 'update_outcome': {
        const updatedOutcome = updateOutcome(action.id, action);
        if (updatedOutcome) results.push({ type: 'update_outcome', outcome: updatedOutcome });
        break;
      }
      case 'create_note': {
        const note = createNote(action);
        results.push({ type: 'create_note', note });
        break;
      }
      default:
        break;
    }
  });
  return results;
}

async function handleApi(req, res, parsedUrl) {
  if (req.method === 'OPTIONS') {
    res.writeHead(204, {
      'Access-Control-Allow-Origin': '*',
      'Access-Control-Allow-Methods': 'GET,POST,PATCH,OPTIONS',
      'Access-Control-Allow-Headers': 'Content-Type',
    });
    return res.end();
  }

  if (req.method === 'GET' && parsedUrl.pathname === '/state/today') {
    return sendJson(res, 200, getTodayState());
  }

  if (req.method === 'POST' && parsedUrl.pathname === '/steps') {
    const body = await parseBody(req);
    const step = createStep(body || {});
    if (!step) return sendJson(res, 400, { error: 'Reached limit of 3 MITs for today' });
    return sendJson(res, 201, step);
  }

  if (req.method === 'PATCH' && parsedUrl.pathname.startsWith('/steps/')) {
    const id = Number(parsedUrl.pathname.split('/')[2]);
    const body = await parseBody(req);
    const step = updateStep(id, body || {});
    if (!step) return sendJson(res, 404, { error: 'Step not found' });
    return sendJson(res, 200, step);
  }

  if (req.method === 'GET' && parsedUrl.pathname === '/outcomes') {
    const { horizon, status } = parsedUrl.query;
    let filtered = [...outcomes];
    if (horizon) filtered = filtered.filter((o) => o.horizon === horizon);
    if (status) filtered = filtered.filter((o) => o.status === status);
    return sendJson(res, 200, filtered);
  }

  if (req.method === 'POST' && parsedUrl.pathname === '/outcomes') {
    const body = await parseBody(req);
    if (body.horizon === 'this_week') {
      const weeklyCount = outcomes.filter((o) => o.horizon === 'this_week').length;
      if (weeklyCount >= 3) return sendJson(res, 400, { error: 'Reached limit of 3 weekly outcomes' });
    }
    const outcome = createOutcome(body || {});
    return sendJson(res, 201, outcome);
  }

  if (req.method === 'PATCH' && parsedUrl.pathname.startsWith('/outcomes/')) {
    const id = Number(parsedUrl.pathname.split('/')[2]);
    const body = await parseBody(req);
    const outcome = updateOutcome(id, body || {});
    if (!outcome) return sendJson(res, 404, { error: 'Outcome not found' });
    return sendJson(res, 200, outcome);
  }

  if (req.method === 'POST' && parsedUrl.pathname === '/notes') {
    const body = await parseBody(req);
    const note = createNote(body || {});
    return sendJson(res, 201, note);
  }

  if (req.method === 'GET' && parsedUrl.pathname === '/backlog') {
    return sendJson(res, 200, {
      weekly_outcomes: outcomes.filter((o) => o.horizon === 'this_week'),
      someday_outcomes: outcomes.filter((o) => o.horizon === 'someday'),
      someday_steps: steps.filter((s) => s.planned_for === 'someday'),
      notes,
    });
  }

  if (req.method === 'POST' && parsedUrl.pathname === '/ai/message') {
    const body = await parseBody(req);
    const plan = simpleActionPlan(body.message || '');
    const applied = applyActions(plan.actions);
    return sendJson(res, 200, { reply: plan.reply, actions: applied, state: getTodayState() });
  }

  return false;
}

function serveStatic(req, res, parsedUrl) {
  let pathname = parsedUrl.pathname === '/' ? '/index.html' : parsedUrl.pathname;
  const filePath = path.join(PUBLIC_DIR, pathname);
  if (!filePath.startsWith(PUBLIC_DIR)) return false;
  if (fs.existsSync(filePath) && fs.statSync(filePath).isFile()) {
    const ext = path.extname(filePath).toLowerCase();
    const type = ext === '.html' ? 'text/html'
      : ext === '.css' ? 'text/css'
      : ext === '.js' ? 'application/javascript'
      : 'text/plain';
    res.writeHead(200, { 'Content-Type': type });
    fs.createReadStream(filePath).pipe(res);
    return true;
  }
  return false;
}

const server = http.createServer(async (req, res) => {
  const parsedUrl = url.parse(req.url, true);
  try {
    const handled = await handleApi(req, res, parsedUrl);
    if (handled !== false) return;
  } catch (err) {
    console.error(err);
    return sendJson(res, 500, { error: 'Server error' });
  }

  const served = serveStatic(req, res, parsedUrl);
  if (!served) {
    res.writeHead(404, { 'Content-Type': 'text/plain' });
    res.end('Not found');
  }
});

// Seed sample data
createOutcome({ title: 'Ship prototype', horizon: 'this_week' });
createOutcome({ title: 'Improve onboarding', horizon: 'someday' });
createStep({ title: 'Sketch UI ideas', planned_for: 'today', time_block: 'morning' });
createStep({ title: 'Reply to emails', planned_for: 'today_extra' });
createNote({ content: 'Remember to demo to Alex on Friday.' });

server.listen(PORT, () => {
  console.log(`Server listening on port ${PORT}`);
});
