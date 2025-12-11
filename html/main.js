// main.js - FINAL VERSION

// --- 1. Global State ---
var rawData = new vis.DataSet([]);
var activeTags = new Set();
var tagColors = {};
var allKnownTags = []; 
var currentSelectedId = null;

// --- 2. Filter Rules ---
const filterRules = function(item) {
    if (!item) return false;
    // Safety: if nothing selected, show nothing
    if (activeTags.size === 0) return false; 
    // If item has no tags, check "Uncategorized"
    if (!item.all_tags || item.all_tags.length === 0) {
        return activeTags.has("Uncategorized");
    }
    // Check if item has ANY active tag
    return item.all_tags.some(tag => activeTags.has(tag));
};

// --- 3. View & Timeline Setup ---
var dataView = new vis.DataView(rawData, { filter: filterRules });
var container = document.getElementById('visualization');
var options = {
    orientation: 'bottom',
    zoomKey: 'ctrlKey',
    horizontalScroll: true,
    stack: true,
    height: '100%',
    width: '100%',
    selectable: true
};

var timeline = new vis.Timeline(container, dataView, options);

// --- 4. Selection & Preview Logic ---
timeline.on('select', function (properties) {
    const selectedIds = properties.items;
    if (selectedIds.length > 0) {
        const id = selectedIds[0];
        const item = rawData.get(id);
        handleNodeSelect(item);
    } else {
        closePreview();
    }
});

function handleNodeSelect(item) {
    currentSelectedId = item.id;
    const panel = document.getElementById('preview-panel');
    document.getElementById('preview-title').innerText = item.content;
    const contentBox = document.getElementById('preview-content');
    contentBox.innerHTML = "<div style='text-align:center; padding:20px; color:#666;'><i class='fas fa-spinner fa-spin'></i> Loading...</div>";
    panel.classList.add('open');

    fetch(`/content?id=${item.id}`)
        .then(res => res.text())
        .then(html => {
            contentBox.innerHTML = html;
            if (window.MathJax) {
                MathJax.typesetPromise([contentBox]);
            }
        })
        .catch(err => {
            contentBox.innerHTML = "<p>Error loading content.</p>";
        });

    highlightNetwork(item);
}

function highlightNetwork(centerItem) {
    document.body.classList.add('focus-mode');
    const neighbors = new Set(centerItem.neighbors || []);
    neighbors.add(centerItem.id);

    const updates = [];
    rawData.forEach(item => {
        const baseClass = (item.className || "").replace(' highlighted', '');
        if (neighbors.has(item.id)) {
            updates.push({ id: item.id, className: baseClass + ' highlighted' });
        } else {
            updates.push({ id: item.id, className: baseClass });
        }
    });
    rawData.update(updates);
}

function closePreview() {
    document.getElementById('preview-panel').classList.remove('open');
    document.body.classList.remove('focus-mode');
    currentSelectedId = null;
    const updates = [];
    rawData.forEach(item => {
        const baseClass = (item.className || "").replace(' highlighted', '');
        updates.push({ id: item.id, className: baseClass });
    });
    rawData.update(updates);
}

function openInEmacs() {
    if (!currentSelectedId) return;
    fetch(`/open?id=${currentSelectedId}`);
}

// --- 5. Data Loading & Colors ---
const palette = ['#bee3f8', '#fed7d7', '#c6f6d5', '#fefcbf', '#e9d8fd', '#fed7e2', '#c4f1f9'];
const borders = ['#3182ce', '#e53e3e', '#38a169', '#d69e2e', '#805ad5', '#d53f8c', '#00b5d8'];

function assignColorToItem(item) {
    const primaryTag = (item.all_tags && item.all_tags.length > 0) ? item.all_tags[0] : "Uncategorized";
    if (!tagColors[primaryTag]) {
        const index = Object.keys(tagColors).length % palette.length;
        tagColors[primaryTag] = { bg: palette[index], border: borders[index] };
    }
    const c = tagColors[primaryTag];
    item.style = `background-color: ${c.bg}; border-color: ${c.border};`;
}

function loadData() {
    console.log("Fetching data...");
    fetch('/data')
        .then(response => response.json())
        .then(data => {
            const uniqueTags = new Set();
            data.forEach(item => {
                if (!item.all_tags || item.all_tags.length === 0) {
                    item.all_tags = ["Uncategorized"];
                }
                item.all_tags.forEach(t => uniqueTags.add(t));
                assignColorToItem(item);
            });

            allKnownTags = [...uniqueTags].sort();
            
            // Initial Load: Select All
            if (activeTags.size === 0) {
                activeTags = new Set(allKnownTags);
            }

            renderFilters();
            
            rawData.clear();
            rawData.add(data);
            timeline.fit();
        })
        .catch(err => console.error(err));
}

// --- 6. Sidebar & Filters ---
function renderFilters() {
    const container = document.getElementById('filter-list');
    if (!container) return; // Safety check

    container.innerHTML = ''; 
    allKnownTags.forEach(tag => {
        const div = document.createElement('div');
        div.className = 'filter-item';
        div.dataset.tag = tag.toLowerCase();

        const checkbox = document.createElement('input');
        checkbox.type = 'checkbox';
        checkbox.id = `cb-${tag}`;
        checkbox.checked = activeTags.has(tag);
        
        checkbox.onchange = function() {
            if (this.checked) activeTags.add(tag);
            else activeTags.delete(tag);
            dataView.refresh(); 
        };

        const label = document.createElement('label');
        label.htmlFor = `cb-${tag}`;
        const color = tagColors[tag] ? tagColors[tag].bg : '#ccc';
        label.innerHTML = `<span style="color:${color}; font-size:20px; vertical-align:middle;">●</span> ${tag}`;
        label.style.cursor = "pointer";
        label.style.marginLeft = "5px";

        div.appendChild(checkbox);
        div.appendChild(label);
        container.appendChild(div);
    });
}

function toggleSidebar() { document.getElementById('sidebar').classList.toggle('open'); }

function filterTagList() {
    const text = document.getElementById('tag-search').value.toLowerCase();
    document.querySelectorAll('.filter-item').forEach(item => {
        item.style.display = item.dataset.tag.includes(text) ? 'flex' : 'none';
    });
}

function toggleAll(enable) {
    if (enable) {
        activeTags = new Set(allKnownTags);
    } else {
        activeTags.clear();
    }
    document.querySelectorAll('#filter-list input').forEach(cb => cb.checked = enable);
    dataView.refresh();
}

// Start
loadData();
