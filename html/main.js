// main.js 

var rawData = new vis.DataSet([]);
var activeTags = new Set();
var excludedTags = new Set(); 
var forcedVisibleIds = new Set();
var allKnownTags = []; 
var currentSelectedId = null;
var pendingRemoveId = null; 

// Config
var showLinks = true;     
var autoOpenPreview = true;
var followModeEnabled = true;
var zoomWindowYears = 5;

// --- 1. Init ---
function loadConfigAndData() {
    fetch('/org-roam-timeline-config').then(r => r.json()).then(config => {
        if (config.theme === 'light') document.body.classList.add('light-mode');
        else document.body.classList.remove('light-mode');
        
        showLinks = config.showLinks;
        followModeEnabled = config.followMode;
        zoomWindowYears = config.zoomWindow;
        autoOpenPreview = config.autoPreview; 
        
        updateButtonStates();
        loadData();
    }).catch(e => loadData());
}

// --- 2. Filters  ---
const filterRules = function(item) {
    if (!item) return false;
    if (forcedVisibleIds.has(item.id)) return true;
    if (item.all_tags && item.all_tags.some(tag => excludedTags.has(tag))) return false; 
    if (activeTags.size === 0) return false;
    if (!item.all_tags || item.all_tags.length === 0) return activeTags.has("Uncategorized");
    return item.all_tags.some(tag => activeTags.has(tag));
};

var dataView = new vis.DataView(rawData, { filter: filterRules });
var container = document.getElementById('visualization');

// --- Tooltip & Options ---
var options = {
    orientation: 'bottom', zoomKey: 'ctrlKey', horizontalScroll: true,
    stack: true, height: '100%', width: '100%', selectable: true, multiselect: false,
    tooltip: { 
        followMouse: true, overflowMethod: 'cap',
        template: function(item, element) {
            const container = document.createElement('div');
            container.style.textAlign = 'left';
            const titleEl = document.createElement('strong');
            titleEl.innerText = item.content;
            titleEl.style.display = 'block'; titleEl.style.fontSize = '14px'; titleEl.style.marginBottom = '4px';
            container.appendChild(titleEl);
            
            const sDate = new Date(item.start);
            const startStr = isNaN(sDate) ? item.start : sDate.getFullYear();
            let dateStr = startStr;
            if (item.end) {
                const eDate = new Date(item.end);
                const endStr = isNaN(eDate) ? item.end : eDate.getFullYear();
                dateStr = `${startStr} — ${endStr}`;
            }
            const dateEl = document.createElement('span');
            dateEl.innerText = dateStr; dateEl.style.color = 'var(--text-muted, #888)'; dateEl.style.fontSize = '12px';
            container.appendChild(dateEl);
            
            if (item.all_tags && item.all_tags.length > 0) {
                const pillContainer = document.createElement('div');
                pillContainer.style.marginTop = '8px';
                const isLight = document.body.classList.contains('light-mode');
                const txtColor = isLight ? '#333' : '#fff';
                item.all_tags.forEach(tag => {
                    const color = stringToColor(tag);
                    const pill = document.createElement('span');
                    pill.innerText = tag;
                    pill.style.display = 'inline-block'; pill.style.backgroundColor = color; pill.style.border = `1px solid ${color}`; pill.style.color = txtColor; pill.style.padding = '2px 8px'; pill.style.borderRadius = '12px'; pill.style.fontSize = '11px'; pill.style.fontWeight = '600'; pill.style.marginRight = '4px'; pill.style.marginTop = '4px';
                    pillContainer.appendChild(pill);
                });
                container.appendChild(pillContainer);
            }
            return container;
        }
    }
};

var timeline = new vis.Timeline(container, dataView, options);

// --- 3. Canvas & Drawing ---
const canvas = document.getElementById('connection-layer');
const ctx = canvas.getContext('2d');
function resizeCanvas() {
    canvas.width = container.offsetWidth;
    canvas.height = container.offsetHeight;
    if (showLinks) requestAnimationFrame(drawConnections);
}
window.addEventListener('resize', resizeCanvas);
setTimeout(resizeCanvas, 500);
timeline.on('changed', drawConnections);
timeline.on('rangechange', drawConnections);

function getCenter(id) {
    const itemDom = document.querySelector(`.vis-item.node-${CSS.escape(id)}`);
    if (!itemDom) return null;
    const containerRect = container.getBoundingClientRect();
    const dot = itemDom.querySelector('.vis-dot');
    if (dot) {
        const r = dot.getBoundingClientRect();
        return { x: r.left - containerRect.left + (r.width / 2), y: r.top - containerRect.top + (r.height / 2) };
    } else {
        const r = itemDom.getBoundingClientRect();
        return { x: r.left - containerRect.left + (r.width / 2), y: r.top - containerRect.top + (r.height / 2) };
    }
}

function drawConnections() {
    ctx.clearRect(0, 0, canvas.width, canvas.height);
    if (!showLinks) return;
    const isLight = document.body.classList.contains('light-mode');
    
    if (currentSelectedId) {
        const centerItem = rawData.get(currentSelectedId);
        if (!centerItem || !centerItem.neighbors) return;
        const start = getCenter(currentSelectedId);
        if (!start) return;
        ctx.strokeStyle = "rgba(81, 175, 239, 0.9)"; ctx.lineWidth = 3; ctx.beginPath();
        centerItem.neighbors.forEach(nid => {
            if (!dataView.get(nid)) return;
            const end = getCenter(nid);
            if (end) { ctx.moveTo(start.x, start.y); ctx.lineTo(end.x, end.y); }
        });
        ctx.stroke();
    } else {
        ctx.strokeStyle = isLight ? "rgba(0,0,0, 0.05)" : "rgba(255,255,255, 0.05)"; ctx.lineWidth = 1; ctx.beginPath();
        const drawnPairs = new Set();
        dataView.forEach(item => {
            if (!item.neighbors) return;
            const start = getCenter(item.id);
            if (!start) return; 
            item.neighbors.forEach(nid => {
                if (!dataView.get(nid)) return;
                const pairKey = [item.id, nid].sort().join('|');
                if (drawnPairs.has(pairKey)) return;
                drawnPairs.add(pairKey);
                const end = getCenter(nid);
                if (end) { ctx.moveTo(start.x, start.y); ctx.lineTo(end.x, end.y); }
            });
        });
        ctx.stroke();
    }
}

// --- 4. Interaction ---
timeline.on('click', function (properties) {
    const clickedId = properties.item;
    if (clickedId) {
        handleNodeSelect(rawData.get(clickedId));
        timeline.setSelection(clickedId, { focus: false });
    } else {
        closePreview();
    }
});
timeline.on('contextmenu', function (props) {
    props.event.preventDefault();
    const id = props.item;
    if (id) {
        const item = rawData.get(id);
        showConfirmModal(id, item.content);
    }
});
function handleNodeSelect(item) {
    currentSelectedId = item.id;
    if (autoOpenPreview) openInPanel(item.id, item.content);
    highlightNetwork(item);
    requestAnimationFrame(drawConnections);
}

// --- 5. RENDER FILTERS ---
function renderFilters() {
    const container = document.getElementById('filter-list');
    if (!container) return;
    container.innerHTML = ''; 
    allKnownTags.forEach(tag => {
        const div = document.createElement('div');
        div.className = 'filter-item';
        div.dataset.tag = tag.toLowerCase();
        
        const left = document.createElement('div');
        left.className = 'filter-left';
        
        const checkbox = document.createElement('input');
        checkbox.type = 'checkbox';
        checkbox.checked = activeTags.has(tag);
        checkbox.style.pointerEvents = "none"; 
        
        const dot = document.createElement('span');
        dot.className = 'filter-dot';
        dot.style.backgroundColor = stringToColor(tag);
        
        const label = document.createElement('span');
        label.innerText = tag;
        
        left.appendChild(checkbox); left.appendChild(dot); left.appendChild(label);
        
        left.onclick = function() {
            if (activeTags.has(tag)) activeTags.delete(tag);
            else activeTags.add(tag);
            if (excludedTags.has(tag)) excludedTags.delete(tag);
            renderFilters(); dataView.refresh();
        };
        
        div.appendChild(left);

        const eye = document.createElement('i');
        if (excludedTags.has(tag)) {
            eye.className = 'fas fa-eye-slash filter-eye blocked'; eye.title = "Unblock this tag";
        } else {
            eye.className = 'fas fa-eye filter-eye'; eye.title = "Hard block (Hide all)";
        }
        
        eye.onclick = function(e) {
            e.stopPropagation(); 
            if (excludedTags.has(tag)) { excludedTags.delete(tag); } 
            else { excludedTags.add(tag); }
            renderFilters(); dataView.refresh();
        };
        
        div.appendChild(eye); container.appendChild(div);
    });
}

// --- 6. Polling & Signals ---
let lastFocusId = null;
setInterval(() => {
    fetch('/org-roam-timeline-current-focus').then(r=>r.json()).then(resp => {
        
        // --- FILTER COMMANDS ---
        if (resp.action === "filter-toggle" && resp.tag) {
            const tag = resp.tag;
            if (activeTags.has(tag)) activeTags.delete(tag);
            else activeTags.add(tag);
            if (excludedTags.has(tag)) excludedTags.delete(tag);
            renderFilters(); dataView.refresh();
        }
        else if (resp.action === "filter-block" && resp.tag) {
            const tag = resp.tag;
            if (excludedTags.has(tag)) excludedTags.delete(tag);
            else excludedTags.add(tag);
            renderFilters(); dataView.refresh();
        }
        else if (resp.action === "filter-reset") { toggleAll(true); excludedTags.clear(); renderFilters(); }
        else if (resp.action === "filter-hide-all") { toggleAll(false); renderFilters(); }

        // --- GLOBAL TOGGLES ---
        else if (resp.action === "toggle-follow") { toggleFollowMode(); }
        else if (resp.action === "toggle-preview") { togglePreviewMode(); } 

        // --- SPECIFIC ACTIONS ---
        else if (resp.action === "zoom-date" && resp.date) { handleDateZoom(resp.date); }
        else if (resp.action === "hide" && resp.id) {
            rawData.remove(resp.id);
            if (currentSelectedId === resp.id) closePreview();
        } 
        else if (resp.action === "focus" && resp.id) { forceShowNode(resp.id); }
        else if (followModeEnabled && resp.action === "follow" && resp.id) {
            if (resp.id !== lastFocusId) {
                lastFocusId = resp.id;
                const item = rawData.get(resp.id);
                if (item) { focusOnNode(resp.id); handleNodeSelect(item); }
            }
        }
    }).catch(e=>{});
}, 1000);

// --- Helpers ---
function handleDateZoom(dateStr) {
    let start, end;
    const parts = dateStr.split('-');
    if (parts.length === 1) { start = new Date(`${parts[0]}-01-01`); end = new Date(`${parts[0]}-12-31 23:59:59`); } 
    else if (parts.length === 2) { start = new Date(`${dateStr}-01`); end = new Date(parts[0], parts[1], 0, 23, 59, 59); } 
    else { start = new Date(`${dateStr} 00:00:00`); end = new Date(`${dateStr} 23:59:59`); }

    if (isNaN(start.getTime())) return;
    const padding = (end.getTime() - start.getTime()) * 0.5;
    timeline.setWindow(new Date(start.getTime() - padding), new Date(end.getTime() + padding), { animation: { duration: 1000 } });

    document.body.classList.add('focus-mode');
    const updates = []; const tStart = start.getTime(); const tEnd = end.getTime();
    rawData.forEach(item => {
        const itemStart = new Date(item.start).getTime(); const itemEnd = item.end ? new Date(item.end).getTime() : itemStart;
        const overlaps = (itemStart <= tEnd) && (itemEnd >= tStart);
        const baseClass = (item.className || "").replace(' highlighted', '');
        if (overlaps) updates.push({ id: item.id, className: baseClass + ' highlighted' });
        else updates.push({ id: item.id, className: baseClass });
    });
    rawData.update(updates); timeline.setSelection([], { focus: false }); closePreview(); 
}

function showConfirmModal(id, title) { pendingRemoveId = id; document.getElementById('modal-title').innerText = `Hide "${title}"?`; document.getElementById('confirm-modal').classList.add('active'); }
function focusOnNode(id) {
    const item = rawData.get(id); if (!item) return;
    let startWindow, endWindow;
    if (item.type === 'point') {
        const center = new Date(item.start); startWindow = new Date(center); startWindow.setFullYear(center.getFullYear() - zoomWindowYears); endWindow = new Date(center); endWindow.setFullYear(center.getFullYear() + zoomWindowYears);
    } else {
        const itemStart = new Date(item.start); const itemEnd = new Date(item.end); startWindow = new Date(itemStart); startWindow.setFullYear(itemStart.getFullYear() - zoomWindowYears); endWindow = new Date(itemEnd); endWindow.setFullYear(itemEnd.getFullYear() + zoomWindowYears);
    }
    timeline.setWindow(startWindow, endWindow, { animation: { duration: 800 } }); timeline.setSelection(id, { focus: false });
}
function highlightNetwork(centerItem) {
    document.body.classList.add('focus-mode'); const neighbors = new Set(centerItem.neighbors || []); neighbors.add(centerItem.id);
    const updates = []; rawData.forEach(item => { const baseClass = (item.className || "").replace(' highlighted', ''); if (neighbors.has(item.id)) updates.push({ id: item.id, className: baseClass + ' highlighted' }); else updates.push({ id: item.id, className: baseClass }); }); rawData.update(updates);
}
function openInPanel(id, title) {
    const panel = document.getElementById('preview-panel'); const contentBox = document.getElementById('preview-content'); const titleHeader = document.getElementById('preview-title');
    titleHeader.innerText = title || "Loading..."; contentBox.innerHTML = "<div style='text-align:center; padding:20px;'><i class='fas fa-spinner fa-spin'></i> Loading...</div>"; panel.classList.add('open');
    fetch(`/org-roam-timeline-content?id=${id}`).then(r=>r.text()).then(html => { contentBox.innerHTML = html.trim().length ? html : "<p><i>No content.</i></p>"; const h1 = contentBox.querySelector('h1.node-title'); if (h1) { titleHeader.innerText = h1.innerText; h1.style.display = 'none'; } else if (!title) { const firstHeader = contentBox.querySelector('h1, h2'); if(firstHeader) titleHeader.innerText = firstHeader.innerText; } if (window.MathJax) MathJax.typesetPromise([contentBox]).catch(e=>{}); }).catch(e => contentBox.innerHTML = "Error.");
}
function closePreview() { document.getElementById('preview-panel').classList.remove('open'); document.body.classList.remove('focus-mode'); currentSelectedId = null; const updates = []; rawData.forEach(item => { updates.push({ id: item.id, className: (item.className||"").replace(' highlighted', '') }); }); rawData.update(updates); requestAnimationFrame(drawConnections); }
function stringToColor(str) { let hash = 0; for (let i=0; i<str.length; i++) hash = str.charCodeAt(i) + ((hash << 5) - hash); const h = Math.abs(hash % 360); const isLight = document.body.classList.contains('light-mode'); return `hsl(${h}, ${isLight?65:55}%, ${isLight?85:30}%)`; }
function assignColorToItem(item) { const tag = (item.all_tags && item.all_tags.length) ? item.all_tags[0] : "Uncategorized"; const bg = stringToColor(tag); const txt = document.body.classList.contains('light-mode') ? '#333' : '#eee'; item.style = `background-color: ${bg}; border-color: ${bg}; color: ${txt};`; const baseClass = (item.className || "").split(' node-')[0]; item.className = `${baseClass} node-${item.id}`; }
function toggleTheme() { document.body.classList.toggle('light-mode'); const updates = []; rawData.forEach(item => { assignColorToItem(item); updates.push(item); }); rawData.update(updates); renderFilters(); requestAnimationFrame(drawConnections); }
function loadData() { fetch('/org-roam-timeline-data').then(r=>r.json()).then(data => { const uniqueTags = new Set(); data.forEach(item => { if(!item.all_tags) item.all_tags = ["Uncategorized"]; item.all_tags.forEach(t=>uniqueTags.add(t)); assignColorToItem(item); delete item.title; }); allKnownTags = [...uniqueTags].sort(); if(activeTags.size===0) activeTags = new Set(allKnownTags); renderFilters(); rawData.clear(); rawData.update(data); timeline.fit(); updateButtonStates(); }); }
function updateButtonStates() { 
    const linkBtn = document.getElementById('link-btn'); if (showLinks) linkBtn.classList.add('active'); else linkBtn.classList.remove('active');
    const prevBtn = document.getElementById('preview-toggle-btn'); if (autoOpenPreview) prevBtn.classList.add('active'); else prevBtn.classList.remove('active');
    const followBtn = document.getElementById('follow-btn'); if (followModeEnabled) followBtn.classList.add('active'); else followBtn.classList.remove('active');
}
function toggleSidebar() { document.getElementById('sidebar').classList.toggle('open'); }
function filterTagList() { const v = document.getElementById('tag-search').value.toLowerCase(); document.querySelectorAll('.filter-item').forEach(i => i.style.display = i.dataset.tag.includes(v)?'flex':'none'); }
function toggleAll(en) { if(en) activeTags = new Set(allKnownTags); else activeTags.clear(); document.querySelectorAll('#filter-list input').forEach(c => c.checked = en); dataView.refresh(); }
function openInEmacs() { if(currentSelectedId) fetch(`/org-roam-timeline-open?id=${currentSelectedId}`); }
function toggleLinks() { showLinks = !showLinks; updateButtonStates(); requestAnimationFrame(drawConnections); }
function togglePreviewMode() { autoOpenPreview = !autoOpenPreview; updateButtonStates(); }
function toggleFollowMode() { followModeEnabled = !followModeEnabled; updateButtonStates(); }

// Check Params
function checkUrlParams() {
    const params = new URLSearchParams(window.location.search);
    const showId = params.get('show');
    if (showId) {
        forceShowNode(showId);
        window.history.replaceState({}, document.title, "/");
    }
}
loadConfigAndData();
checkUrlParams();
