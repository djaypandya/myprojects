const views = document.querySelectorAll('.view');
const navButtons = document.querySelectorAll('.nav-btn');
const chatThread = document.getElementById('chat-thread');
const chatForm = document.getElementById('chat-form');
const chatInput = document.getElementById('chat-input');

const todayMits = document.getElementById('today-mits');
const todayExtra = document.getElementById('today-extra');
const currentStepEl = document.getElementById('current-step');

const weeklyOutcomesEl = document.getElementById('weekly-outcomes');
const somedayOutcomesEl = document.getElementById('someday-outcomes');
const somedayStepsEl = document.getElementById('someday-steps');

function switchView(target) {
  views.forEach((view) => view.classList.add('hidden'));
  document.getElementById(target).classList.remove('hidden');
  navButtons.forEach((btn) => btn.classList.toggle('active', btn.dataset.view === target));
}

navButtons.forEach((btn) => btn.addEventListener('click', () => switchView(btn.dataset.view)));

function renderMessage(text, type = 'ai') {
  const div = document.createElement('div');
  div.className = `message ${type}`;
  div.textContent = text;
  chatThread.appendChild(div);
  chatThread.scrollTop = chatThread.scrollHeight;
}

async function fetchJSON(url, options = {}) {
  const res = await fetch(url, {
    headers: { 'Content-Type': 'application/json' },
    ...options,
  });
  if (!res.ok) throw new Error('Request failed');
  return res.json();
}

async function loadToday() {
  const state = await fetchJSON('/state/today');
  renderToday(state);
}

function renderToday(state) {
  currentStepEl.innerHTML = '';
  if (state.current_step) {
    const step = state.current_step;
    const card = document.createElement('div');
    card.className = 'card';
    card.innerHTML = `<div class="section-title"><span class="badge">Focus</span><span>${step.time_block || ''}</span></div><h3>${step.title}</h3>`;
    const actions = document.createElement('div');
    actions.className = 'actions';
    actions.innerHTML = `
      <button class="primary" data-action="done" data-id="${step.id}">Done</button>
      <button data-action="later" data-id="${step.id}">Later</button>
    `;
    actions.addEventListener('click', handleStepAction);
    card.appendChild(actions);
    currentStepEl.appendChild(card);
  } else {
    currentStepEl.innerHTML = '<div class="card"><div class="muted">No active step. Choose an MIT to start.</div></div>';
  }

  todayMits.innerHTML = '';
  todayExtra.innerHTML = '';

  state.today_steps
    .filter((s) => s.planned_for === 'today')
    .forEach((step) => todayMits.appendChild(stepCard(step)));

  state.today_steps
    .filter((s) => s.planned_for === 'today_extra')
    .forEach((step) => todayExtra.appendChild(stepCard(step)));

  if (!todayMits.childElementCount) todayMits.innerHTML = '<div class="empty">No MITs yet.</div>';
  if (!todayExtra.childElementCount) todayExtra.innerHTML = '<div class="empty">No extras.</div>';
}

function stepCard(step) {
  const card = document.createElement('div');
  card.className = 'card';
  card.innerHTML = `
    <div class="section-title"><span class="badge">${step.planned_for === 'today' ? 'MIT' : 'Extra'}</span><span class="muted">${step.time_block || ''}</span></div>
    <h3>${step.title}</h3>
    <p class="muted">Status: ${step.status}</p>
  `;
  const actions = document.createElement('div');
  actions.className = 'actions';
  actions.innerHTML = `
    <button class="primary" data-action="start" data-id="${step.id}">Start</button>
    <button data-action="done" data-id="${step.id}">Done</button>
    <button data-action="later" data-id="${step.id}">Later</button>
  `;
  actions.addEventListener('click', handleStepAction);
  card.appendChild(actions);
  return card;
}

async function handleStepAction(event) {
  const action = event.target.dataset.action;
  if (!action) return;
  const id = event.target.dataset.id;
  let updates = {};
  if (action === 'start') updates = { status: 'doing' };
  if (action === 'done') updates = { status: 'done' };
  if (action === 'later') updates = { planned_for: 'tomorrow', status: 'todo' };
  await fetchJSON(`/steps/${id}`, { method: 'PATCH', body: JSON.stringify(updates) });
  loadToday();
}

async function loadBacklog() {
  const data = await fetchJSON('/backlog');
  renderBacklog(data);
}

function renderBacklog(data) {
  weeklyOutcomesEl.innerHTML = '';
  data.weekly_outcomes.forEach((outcome) => weeklyOutcomesEl.appendChild(outcomeCard(outcome, true)));
  if (!weeklyOutcomesEl.childElementCount) weeklyOutcomesEl.innerHTML = '<div class="empty">No weekly outcomes yet.</div>';

  somedayOutcomesEl.innerHTML = '';
  data.someday_outcomes.forEach((outcome) => somedayOutcomesEl.appendChild(outcomeCard(outcome, false)));
  if (!somedayOutcomesEl.childElementCount) somedayOutcomesEl.innerHTML = '<div class="empty">No someday outcomes.</div>';

  somedayStepsEl.innerHTML = '';
  data.someday_steps.forEach((step) => {
    const card = document.createElement('div');
    card.className = 'card';
    card.innerHTML = `<h3>${step.title}</h3><p class="muted">Status: ${step.status}</p>`;
    somedayStepsEl.appendChild(card);
  });
  if (!somedayStepsEl.childElementCount) somedayStepsEl.innerHTML = '<div class="empty">No someday steps.</div>';
}

function outcomeCard(outcome, isWeekly) {
  const card = document.createElement('div');
  card.className = 'card';
  card.innerHTML = `<h3>${outcome.title}</h3><p class="muted">${outcome.description || 'No description yet'}</p>`;
  const actions = document.createElement('div');
  actions.className = 'actions';
  if (!isWeekly) {
    const btn = document.createElement('button');
    btn.textContent = 'Make Weekly';
    btn.dataset.id = outcome.id;
    btn.addEventListener('click', () => promoteOutcome(outcome.id));
    actions.appendChild(btn);
  } else {
    const tag = document.createElement('span');
    tag.className = 'badge';
    tag.textContent = 'This Week';
    actions.appendChild(tag);
  }
  card.appendChild(actions);
  return card;
}

async function promoteOutcome(id) {
  await fetchJSON(`/outcomes/${id}`, { method: 'PATCH', body: JSON.stringify({ horizon: 'this_week' }) });
  loadBacklog();
}

chatForm.addEventListener('submit', async (e) => {
  e.preventDefault();
  const text = chatInput.value.trim();
  if (!text) return;
  renderMessage(text, 'user');
  chatInput.value = '';

  try {
    const res = await fetchJSON('/ai/message', { method: 'POST', body: JSON.stringify({ message: text }) });
    renderMessage(res.reply, 'ai');
    loadToday();
    loadBacklog();
  } catch (err) {
    renderMessage('Something went wrong. Try again.', 'ai');
  }
});

function init() {
  renderMessage('Welcome! Tell me what you want to focus on today.', 'ai');
  loadToday();
  loadBacklog();
}

init();
