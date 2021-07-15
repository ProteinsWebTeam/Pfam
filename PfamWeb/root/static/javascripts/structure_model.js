const IP_PFAM_FAMILY_API = "https://www.ebi.ac.uk/interpro/wwwapi/entry/pfam/";
const IP_PFAM_FAMILY_WEB = "https://www.ebi.ac.uk/interpro/entry/pfam/";
const VIEWPORT_ID = "structure_viewport";
const BLUE = "1148136";

const showStructure = function(acc, chain, pdbResKey, urlString) {
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

const showStructureInStructurePage = function(accession) {
  try {
    const container = document.getElementById('ngl-scontainer');
    container.style.display = "block";

    const viewer = document.createElement("div");
    viewer.id = VIEWPORT_ID;
    viewer.style.minHeight = "400px";
    viewer.style.minWidth = "300px";

    const nglContainer = document.getElementById("ngl-viewport");
    nglContainer.replaceChildren(viewer);

    var stage = new NGL.Stage(VIEWPORT_ID);
    stage.setParameters({ backgroundColor: "white"});
    // Handle window resizing
    window.addEventListener( "resize", function( event ){
        stage.handleResize();
    }, false );
    stage.loadFile(
      `https://mmtf.rcsb.org/v1.0/full/${accession}`,
      { "ext": "mmtf" }
    ).then(function (component) {
      component.addRepresentation('cartoon');
      component.autoView();

      // hide spinner
      const spinner = document.getElementById("ngl-spinner");
      spinner.style.display = "none";

      // Prevent scrolling when touching the canvas
      const canvas = document.getElementById(VIEWPORT_ID).firstChild;
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
    });
  } catch(e) {
    const errorContainer = document.getElementById("ngl-viewport");
    errorContainer.innerHTML = `<p>Something went wrong whilst fetching structure
    data. Please try again later, or if the problem persists contact us on
    pfam-help@ebi.ac.uk.</p>`;
    console.log("Error: " + e);
  }
}
