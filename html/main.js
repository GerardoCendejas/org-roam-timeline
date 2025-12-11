// main.js

// --- 1. Global State ---
var rawData = new vis.DataSet([]);
var activeTags = new Set();
var tagColors = {};
var allKnownTags = []; 

// --- 2. The Filter Logic (The Fix) ---
const filterRules = function(item) {
    if (!item) return false;
    
    // BUG FIX: Removed the "if activeTags.size === 0 return true" line.
    // Now, if you select "None", activeTags is empty, and this returns false (hiding items).
    
    // If item has no tags, treat as "Uncategorized"
    // (We handle this in loadData, but safe to check here)
    if (!item.all_tags || item.all_tags.length === 0) {
        return activeTags.has("Uncategorized");
    }

    // Return TRUE only if the item matches at least one active tag
    return item.all_tags.some(tag => activeTags.has(tag));
};

// --- 3. The DataView ---
var dataView = new vis.DataView(rawData, { 
    filter: filterRules 
});

// --- 4. Timeline Setup ---
var container = document.getElementById('visualization');
var options = {
    orientation: 'bottom',
    zoomKey: 'ctrlKey',
    horizontalScroll: true,
    stack: true,
    height: '100%',
    width: '100%'
};

var timeline = new vis.Timeline(container, dataView, options);

// Pretty Pastels Palette
const palette = ['#bee3f8', '#fed7d7', '#c6f6d5', '#fefcbf', '#e9d8fd', '#fed7e2', '#c4f1f9'];
const borders = ['#3182ce', '#e53e3e', '#38a169', '#d69e2e', '#805ad5', '#d53f8c', '#00b5d8'];

// --- 5. Data Loading ---
function loadData() {
    console.log("Fetching data...");
    
    fetch('/data')
        .then(response => response.json())
        .then(data => {
            const uniqueTags = new Set();

            data.forEach(item => {
                // Ensure tags exist
                if (!item.all_tags || item.all_tags.length === 0) {
                    item.all_tags = ["Uncategorized"];
                }
                item.all_tags.forEach(t => uniqueTags.add(t));
                assignColorToItem(item);
            });

            allKnownTags = [...uniqueTags].sort();
            
            // Only select all on FIRST load
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

// --- 6. Helpers ---

function assignColorToItem(item) {
    const primaryTag = item.all_tags[0];
    if (!tagColors[primaryTag]) {
        const index = Object.keys(tagColors).length % palette.length;
        tagColors[primaryTag] = { bg: palette[index], border: borders[index] };
    }
    const c = tagColors[primaryTag];
    item.style = `background-color: ${c.bg}; border-color: ${c.border};`;
}

function renderFilters() {
    const container = document.getElementById('filter-list');
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

// --- 7. Sidebar & Bulk Actions ---

function toggleSidebar() {
    document.getElementById('sidebar').classList.toggle('open');
}

function filterTagList() {
    const text = document.getElementById('tag-search').value.toLowerCase();
    document.querySelectorAll('.filter-item').forEach(item => {
        item.style.display = item.dataset.tag.includes(text) ? 'flex' : 'none';
    });
}

// FIX: Improved Toggle All Logic
function toggleAll(enable) {
    if (enable) {
        // 1. Fill the Set with everything
        activeTags = new Set(allKnownTags);
    } else {
        // 2. Clear the Set completely
        activeTags.clear();
    }
    
    // 3. Update Visual Checkboxes
    document.querySelectorAll('#filter-list input[type="checkbox"]').forEach(cb => {
        cb.checked = enable;
    });
    
    // 4. Force Timeline Update
    dataView.refresh();
}

// Start
loadData();
