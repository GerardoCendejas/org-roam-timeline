// main.js - FINAL

var rawData = new vis.DataSet([]);
var activeTags = new Set();
var allKnownTags = []; 
var currentSelectedId = null;
var showLinks = true;     
var autoOpenPreview = true;
var pendingRemoveId = null; 

// --- Filter ---
const filterRules = function(item) {
    if (!item) return false;
    if (activeTags.size === 0) return false;
    if (!item.all_tags || item.all_tags.length === 0) return activeTags.has("Uncategorized");
    return item.all_tags.some(tag => activeTags.has(tag));
};

var dataView = new vis.DataView(rawData, { filter: filterRules });
var container = document.getElementById('visualization');
var options = {
    orientation: 'bottom', zoomKey: 'ctrlKey', horizontalScroll: true,
    stack: true, height: '100%', width: '100%', selectable: true, multiselect: false
};
var timeline = new vis.Timeline(container, dataView, options);

// --- CANVAS ---
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
    ctx.strokeStyle = isLight ? "rgba(49, 130, 206, 0.4)" : "rgba(81, 175, 239, 0.4)";
    ctx.lineWidth = 2;
    ctx.beginPath();
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
            if (end) {
                ctx.moveTo(start.x, start.y);
                ctx.lineTo(end.x, end.y);
            }
        });
    });
    ctx.stroke();
}

// --- Interaction ---
timeline.on('click', function (properties) {
    const clickedId = properties.item;
    if (clickedId) {
        handleNodeSelect(rawData.get(clickedId));
        timeline.setSelection(clickedId, { focus: false });
    } else {
        closePreview();
    }
});

// REMOVAL CONFIRMATION
timeline.on('contextmenu', function (props) {
    props.event.preventDefault();
    const id = props.item;
    if (id) {
        const item = rawData.get(id);
        showConfirmModal(id, item.content);
    }
});

function showConfirmModal(id, title) {
    pendingRemoveId = id;
    document.getElementById('modal-title').innerText = `Hide "${title}"?`;
    document.getElementById('confirm-modal').classList.add('active');
}

window.closeModal = function(confirmed) {
    document.getElementById('confirm-modal').classList.remove('active');
    if (confirmed && pendingRemoveId) {
        rawData.remove(pendingRemoveId);
        if (currentSelectedId === pendingRemoveId) closePreview();
    }
    pendingRemoveId = null;
}

function handleNodeSelect(item) {
    currentSelectedId = item.id;
    if (autoOpenPreview) openPreviewPanel(item);
    highlightNetwork(item);
    requestAnimationFrame(drawConnections);
}

function focusOnNode(id) {
    const item = rawData.get(id);
    if (!item) return;
    if (item.type === 'point') {
        const d = new Date(item.start);
        const start = new Date(d); start.setFullYear(d.getFullYear() - 5);
        const end = new Date(d); end.setFullYear(d.getFullYear() + 5);
        timeline.setWindow(start, end, { animation: { duration: 800 } });
    } else {
        timeline.focus(id, { animation: { duration: 800 } });
    }
    timeline.setSelection(id, { focus: false });
}

function highlightNetwork(centerItem) {
    document.body.classList.add('focus-mode');
    const neighbors = new Set(centerItem.neighbors || []);
    neighbors.add(centerItem.id);
    const updates = [];
    rawData.forEach(item => {
        const baseClass = (item.className || "").replace(' highlighted', '');
        if (neighbors.has(item.id)) updates.push({ id: item.id, className: baseClass + ' highlighted' });
        else updates.push({ id: item.id, className: baseClass });
    });
    rawData.update(updates);
}

function openPreviewPanel(item) {
    const panel = document.getElementById('preview-panel');
    document.getElementById('preview-title').innerText = item.content;
    const contentBox = document.getElementById('preview-content');
    contentBox.innerHTML = "<div style='text-align:center; padding:20px;'><i class='fas fa-spinner fa-spin'></i> Loading...</div>";
    panel.classList.add('open');
    fetch(`/content?id=${item.id}`).then(r=>r.text()).then(html => {
        contentBox.innerHTML = html.trim().length ? html : "<p><i>No content.</i></p>";
        if (window.MathJax) MathJax.typesetPromise([contentBox]).catch(e=>{});
    }).catch(e => contentBox.innerHTML = "Error.");
}

function closePreview() {
    document.getElementById('preview-panel').classList.remove('open');
    document.body.classList.remove('focus-mode');
    currentSelectedId = null;
    const updates = [];
    rawData.forEach(item => {
        updates.push({ id: item.id, className: (item.className||"").replace(' highlighted', '') });
    });
    rawData.update(updates);
}

// --- Helpers ---
function stringToColor(str) {
    let hash = 0;
    for (let i=0; i<str.length; i++) hash = str.charCodeAt(i) + ((hash << 5) - hash);
    const h = Math.abs(hash % 360);
    const isLight = document.body.classList.contains('light-mode');
    return `hsl(${h}, ${isLight?65:55}%, ${isLight?85:30}%)`;
}
function assignColorToItem(item) {
    const tag = (item.all_tags && item.all_tags.length) ? item.all_tags[0] : "Uncategorized";
    const bg = stringToColor(tag);
    const txt = document.body.classList.contains('light-mode') ? '#333' : '#eee';
    item.style = `background-color: ${bg}; border-color: ${bg}; color: ${txt};`;
    const baseClass = (item.className || "").split(' node-')[0];
    item.className = `${baseClass} node-${item.id}`;
}
function toggleTheme() {
    document.body.classList.toggle('light-mode');
    const updates = [];
    rawData.forEach(item => {
        assignColorToItem(item);
        updates.push(item);
    });
    rawData.update(updates);
    renderFilters();
    if (showLinks) drawConnections();
}
function loadData() {
    fetch('/data').then(r=>r.json()).then(data => {
        const uniqueTags = new Set();
        data.forEach(item => {
            if(!item.all_tags) item.all_tags = ["Uncategorized"];
            item.all_tags.forEach(t=>uniqueTags.add(t));
            assignColorToItem(item);
        });
        allKnownTags = [...uniqueTags].sort();
        if(activeTags.size===0) activeTags = new Set(allKnownTags);
        renderFilters();
        rawData.clear(); rawData.add(data);
        timeline.fit();
        updateButtonStates();
    });
}
function updateButtonStates() {
    const linkBtn = document.getElementById('link-btn');
    if (showLinks) linkBtn.classList.add('active'); else linkBtn.classList.remove('active');
    const prevBtn = document.getElementById('preview-toggle-btn');
    if (autoOpenPreview) prevBtn.classList.add('active'); else prevBtn.classList.remove('active');
}
function renderFilters() {
    const container = document.getElementById('filter-list');
    if (!container) return;
    container.innerHTML = ''; 
    allKnownTags.forEach(tag => {
        const div = document.createElement('div');
        div.className = 'filter-item';
        div.dataset.tag = tag.toLowerCase();
        const checkbox = document.createElement('input');
        checkbox.type = 'checkbox';
        checkbox.checked = activeTags.has(tag);
        checkbox.onchange = function() {
            if (this.checked) activeTags.add(tag); else activeTags.delete(tag);
            dataView.refresh(); 
        };
        const label = document.createElement('label');
        label.innerHTML = `<span style="color:${stringToColor(tag)}; margin-right:8px; font-size:18px;">●</span> ${tag}`;
        label.style.cursor="pointer";
        div.appendChild(checkbox); div.appendChild(label);
        container.appendChild(div);
    });
}
function toggleSidebar() { document.getElementById('sidebar').classList.toggle('open'); }
function filterTagList() {
    const v = document.getElementById('tag-search').value.toLowerCase();
    document.querySelectorAll('.filter-item').forEach(i => i.style.display = i.dataset.tag.includes(v)?'flex':'none');
}
function toggleAll(en) {
    if(en) activeTags = new Set(allKnownTags); else activeTags.clear();
    document.querySelectorAll('#filter-list input').forEach(c => c.checked = en);
    dataView.refresh();
}
function openInEmacs() { if(currentSelectedId) fetch(`/open?id=${currentSelectedId}`); }
function toggleLinks() {
    showLinks = !showLinks;
    updateButtonStates();
    if(showLinks) requestAnimationFrame(drawConnections); 
    else ctx.clearRect(0, 0, canvas.width, canvas.height);
}
function togglePreviewMode() {
    autoOpenPreview = !autoOpenPreview;
    updateButtonStates();
}

// SMART POLL (Handles Follow Mode & Explicit Show)
let lastFocusId = null;
setInterval(() => {
    fetch('/current-focus')
        .then(r => r.text())
        .then(id => {
            if (id && id.length > 0) {
                // If it's a new ID, we must handle it
                if (id !== lastFocusId) {
                    lastFocusId = id;
                    
                    const item = rawData.get(id);
                    if (item) {
                        // It's already here -> Focus
                        focusOnNode(id);
                        handleNodeSelect(item);
                    } else {
                        // IT'S MISSING! Fetch and add it.
                        console.log("Fetching missing node:", id);
                        fetch(`/node-data?id=${id}`)
                            .then(r => r.json())
                            .then(newItem => {
                                if (newItem && newItem.id) {
                                    if(!newItem.all_tags) newItem.all_tags = ["Uncategorized"];
                                    assignColorToItem(newItem);
                                    rawData.add(newItem);
                                    // Now focus
                                    focusOnNode(newItem.id);
                                    handleNodeSelect(newItem);
                                }
                            });
                    }
                }
            }
        })
        .catch(e => {}); // Silent fail
}, 1000); // Check every 1 second

loadData();
