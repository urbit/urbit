Idiomorph.defaults.ignoreActive = true;
Idiomorph.defaults.callbacks.beforeAttributeUpdated = (name, node, type) => {
  if (node.hasAttribute('morph-retain')) {
    let ribs = node.getAttribute('morph-retain').split(',').map(t => t.trim());
    if (ribs.includes(name)) {
      return false;
    }
  }
}
Idiomorph.defaults.callbacks.beforeNodeMorphed = (oldNode, newNode) => {
  if (oldNode?.nodeName !== "#text") {
    if (oldNode.hasAttribute('morph-no-swap') && oldNode.id === newNode.id) {
      return false;
    }
    else if (
      newNode.hasAttribute('morph-if-class') &&
      !oldNode.classList.contains(newNode.getAttribute('morph-if-class'))
    ) {
      return false;
    }
  }
}
