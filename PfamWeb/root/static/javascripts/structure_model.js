const IP_PFAM_FAMILY_API = "https://www.ebi.ac.uk/interpro/wwwapi/entry/pfam/";
const IP_PFAM_FAMILY_WEB = "https://www.ebi.ac.uk/interpro/entry/pfam/";
const VIEWPORT_ID = "structure_viewport";
const BLUE = "1148136";

const addMolsterComponentObserver = function(selectorName, name, url) {
  console.log(`addMolsterComponentObserver: ${name} : ${url}`);
  try {
    const element = document.getElementById(name);
    const cStyle = window.getComputedStyle(element);
    const selector = document.getElementById(selectorName);
    const obs = new MutationObserver((entries, o) => {
      for (const e of entries) {
        if (e.type = "attributes") {
          // console.log(`${e.target.id} Mutated!!!`);
          // console.log(e.type);
          // console.log(e.target.classList);
          // console.log(cStyle.visibility);
          if (cStyle.visibility === 'visible') {
            element.setAttribute('url', url);
            obs.disconnect();
          }
        }
      }
    });
    obs.observe(selector, {attributes: true, childList: false, characterData: false});
  } catch(e) {
    console.log(`Error attaching observer: ${e}`);
  }
};

const showStructure = function(family, acc, chain, pdbResKey, urlString) {
  try {
    // resize and add canvas behaviour
    const canvas = document.getElementById('mol-canvas');
    canvas.style.width = "80vh";
    canvas.style.height = "60vh";

    // Prevent scrolling when touching the canvas
    document.body.addEventListener("wheel", function (e) {
      if (e.target == canvas) {
        e.preventDefault();
      }
    }, {passive: false});
    document.body.addEventListener("touchstart", function (e) {
      if (e.target == canvas) {
        e.preventDefault();
      }
    }, {passive: false});
    document.body.addEventListener("touchend", function (e) {
      if (e.target == canvas) {
        e.preventDefault();
      }
    }, {passive: false});
    document.body.addEventListener("touchmove", function (e) {
      if (e.target == canvas) {
        e.preventDefault();
      }
    }, {passive: false});

    // setup molstar parameters
    const accession = acc.toLowerCase();
    const url = String(eval('`'+urlString+'`'));
    const [start, end] = pdbResKey.split(" - ");
    const locations = [{
      chain: chain,
      start: start,
      end: end,
      colour: BLUE
    }];
    const container = document.getElementById('ngl-container');
    container.style.display = "block";
    const title = document.getElementById("ngl-title");
    title.innerHTML = `<h1>${accession}</h1>`;

    const viewer = document.getElementById('pfam-molstar');
    if (family) {
      viewer.setAttribute('accession', family);
    }
    viewer.setAttribute('type', 'structure');
    viewer.setAttribute('url', url);
    viewer.setAttribute('locations', JSON.stringify(locations));

    // hide spinner
    const spinner = document.getElementById("ngl-spinner");
    spinner.style.display = "none";

  } catch(e) {
    const spinner = document.getElementById("ngl-spinner");
    spinner.style.display = "none";
    const errorContainer = document.getElementById("molstar-message");
    errorContainer.innerHTML = `<p>Something went wrong whilst fetching structure
    data. Please try again later, or if the problem persists contact us on
    pfam-help@ebi.ac.uk.</p>`;
    console.log("Error: " + e);
  }
};

const hideStructure = function() {
  try {
    const container = document.getElementById('ngl-container');
    container.style.display = "none";
  } catch (e) {
    console.log("Error: " + e);
  }
};
