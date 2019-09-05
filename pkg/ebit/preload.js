/*
    All of the Node.js APIs are available in the preload process.
*/
window.addEventListener('DOMContentLoaded', () => {
  const replaceText = (selector, text) => {
    const element = document.getElementById(selector)
    if (element) element.innerText = text
  };

  for (const type of ['chrome', 'node', 'electron']) {
    replaceText(`${type}-version`, process.versions[type]);
  };

  console.log("Dom nodes replaced.");
});

console.log("PRELOAD");
