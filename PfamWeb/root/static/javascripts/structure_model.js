const IP_PFAM_FAMILY_API = "https://www.ebi.ac.uk/interpro/wwwapi/entry/pfam/";
const IP_PFAM_FAMILY_WEB = "https://www.ebi.ac.uk/interpro/entry/pfam/";
const VIEWPORT_ID = "structure_viewport";
const BLUE = "1148136";

const getInterProStructureModelLink = async function(accession) {
  const url = new URL(accession, IP_PFAM_FAMILY_API);
  let promise = fetch(url);
  let response = await(promise);
  if (response.ok) {
    const json = await response.json();
    if (json?.metadata?.counters?.structural_models > 0) {
      addStructureTabToPage(accession);
    }
  } else {
    console.log("WARNING: Failed to fetch " + url);
  }
};

const addStructureTabToPage = function(accession) {
  try {
    const structureModelId = "tabview=tab9";
    const structureModelSelectorId = "structureModelSelector"
    // create navigation link
    const structureModelSelector = document.createElement("LI");
    structureModelSelector.id = structureModelSelectorId;
    const link = document.createElement("A");
    //link.href = "#" + structureModelId;
    link.text = "Structural Model";
    structureModelSelector.append(link);

    // add to navigation link
    const structure = document.getElementById('pdbBlockSelector');
    const sidebar = document.getElementById("sidebar");
    if (structure && sidebar) {
      structure.parentElement.appendChild(structureModelSelector);
      sidebar.addEventListener("click", (e) => {
        e.preventDefault();
        const target = e.target.parentElement;
        const selectors = target.parentElement.children;
        const structureModelSection = document.getElementById(structureModelId);
        if (target.id === structureModelSelectorId) {
          //container.style.visibility = "visible";
          const content = document.getElementById("content").children;
          for (const section of content) {
            if (section.id != structureModelSelectorId) {
              section.classList.add("yui-hidden");
            }
          }
          structureModelSection.classList.remove("yui-hidden");
          structureModelSection.title = "active";
          for (const selector of selectors) {
              selector.classList.remove("selected");
              selector.title = null;
              target.title = "active";
              target.classList.add("selected");
          }
        } else {
          //container.style.visibility = "hidden";
          structureModelSection.classList.add("yui-hidden");
          structureModelSection.title = null;
          const selector = document.getElementById(structureModelSelectorId);
          selector.classList.remove("selected");
          selector.title = null;
        }

      });
    }
    const url = new URL(`${accession}/model`, IP_PFAM_FAMILY_WEB);

    // create container for structure model divs
    const structureModelContainer = document.createElement("DIV");
    structureModelContainer.id = structureModelId;
    structureModelContainer.classList.add("block");
    addStructureModelContent(structureModelContainer, accession);

    // add to doc
    const detailsContainer = document.getElementById("content");
    if (detailsContainer) {
      detailsContainer.appendChild(structureModelContainer);
    }
    if (structureModelSelector.classList.contains("selected")) {
      //structureModelContainer.style.visibility = "visible";
      structureModelContainer.classList.remove("yui-hidden");
    } else {
      //structureModelContainer.style.visibility = "hidden";
      structureModelContainer.classList.add("yui-hidden");
    }

    // attach NGL structure viewer to initialised page
    var stage = new NGL.Stage(VIEWPORT_ID);
    stage.setParameters({ backgroundColor: "white"});
    // Handle window resizing
    window.addEventListener( "resize", function( event ){
        stage.handleResize();
    }, false );
    stage.loadFile(
      `https://www.ebi.ac.uk/interpro/wwwapi//entry/pfam/${accession}/?model%3Astructure=`,
      { "ext": "pdb" }
    ).then(function (component) {
      component.addRepresentation('cartoon', { colorScheme: "chainid" });
      component.autoView();
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

  } catch (e) {
    console.log("Error:" + e);
  }
};

const addStructureModelContent = function(structureModelContainer, accession) {
  // create title
  const titleContainer = createTitle();
  // create content
  const textContainer = createContainer(accession);
  // add both to supplied element
  structureModelContainer.appendChild(titleContainer);
  structureModelContainer.appendChild(textContainer);
}

const createTitle = function() {
  const title = document.createElement("H1");
  title.innerHTML = "Structural Model";
  const titleContainer = document.createElement("DIV");
  titleContainer.classList.add("handle");
  titleContainer.appendChild(title);
  return titleContainer;
}

const createContainer = function(accession) {
  const intro = document.createElement("P");
  intro.innerHTML = `The structural model below was generated by the
  <a href="https://www.bakerlab.org/">Baker</a> group with the trRosetta
  software using the Pfam UniProt multiple sequence alignment.`;

  const viewer = document.createElement("div");
  viewer.id = VIEWPORT_ID;
  viewer.style.margin = "auto";
  viewer.style.height = "50vh";
  viewer.style.width = "75vw";

  const links = document.createElement("P");
  links.innerHTML = `
  <p>The InterPro website shows the contact map for the Pfam SEED alignment.
  Hovering or clicking on a contact position will highlight its connection to
  other residues in the alignment, as well as on the 3D structure.
  <ul>
    <li><a href="${IP_PFAM_FAMILY_WEB}/${accession}/model" target="_blank">
    View the contact map and structural model in InterPro</a></li>
    <li><a href="${IP_PFAM_FAMILY_API}/${accession}/?model:structure"
    filename="${accession}_prediction.pdb" target="_blank">
    Download the model in PDB format</a></li>
    <li><a href="http://ftp.ebi.ac.uk/pub/databases/Pfam/baker/" target="_blank">
    Download all the data from the Pfam FTP site</a></li>
  </ul>

  <div class="citation">
    <span class="title">
      <a class="ext" href="https://doi.org/10.1073/pnas.1914677117">
        Improved protein structure prediction using predicted interresidue
        orientations.
      </a>
    </span>
    <span class="authors">
      Jianyi Yang, Ivan Anishchenko, Hahnbeom Park, Zhenling Peng,
      Sergey Ovchinnikov, David Baker
    </span>
    <span class="ref">
      <span class="jrnl">Proceedings of the National Academy of Sciences</span>
      Jan 2020, 117 (3) 1496-1503;
      DOI: 10.1073/pnas.1914677117;
    </span>
  </div>`

  const textContainer = document.createElement("DIV");
  textContainer.classList.add("blockContent");
  textContainer.appendChild(intro);
  textContainer.appendChild(viewer);
  textContainer.appendChild(links);
  return textContainer;
}

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
