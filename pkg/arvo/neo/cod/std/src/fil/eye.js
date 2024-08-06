//  a keyable overlay
function handleKey(e) {
  let focused = document.activeElement;
  let textarea = ['TEXTAREA'].includes(focused.nodeName)
  let textinput = ['text', 'number', 'email', 'password'].includes(focused.type);
  if (textarea || textinput) {
    if (e.key === 'Escape') {
      document.activeElement.blur();
    }
    return;
  }
  else if (e.key === 'Escape') {
    closeEye();
    document.activeElement.blur();
  }
  else if (e.key === ' ') {
    e.preventDefault();
    if (window.eye.open) {
      closeEye();
    } else {
      openEye();
    }
  } else if (e.key === 'Escape') {
    e.preventDefault();
    document.activeElement.blur();
  } else if (!e.ctrlKey && !e.metaKey && !e.altKey) {
    if (window.eye?.open) {
      e.preventDefault();
      let area = window.eye?.spots?.filter(s => s[0][0] === e.key);
      if (area.length === 1) {
        let btn = area[0][1];
        btn.click();
        btn.focus();
        closeEye();
      }
      else if (!!area.length) {
        let news = area.map(c => (c[0].length < 2) ? c : [c[0].slice(1), c[1]]);
        window.eye.spots = news;
        closeEye(true);
        openEye();
      } else {
        closeEye();
      }
    }
  }
}
function openEye() {
  window.eye.open = true;
  document.getElementById('eye').classList.remove('hidden');
  buildGazeSpots();
}
function closeEye(keep) {
  window.eye.open = false;
  if (!keep) {
    window.eye.spots = null;
  }
  document.getElementById('eye').classList.add('hidden');
  document.querySelectorAll('.gaze').forEach(g => g.remove());
}
function buildGazeSpots() {
  let buttons = window.eye?.spots?.map(s => s[1]) ||
    document.querySelectorAll(
      'a, button, summary, [role="button"], input, textarea, .clickable'
    );
  let chars = ['a', 's', 'd', 'f', 'k', 'm', 'n', 'r', 't', 'y', 'u', 'i', 'c', 'v', 'b'];
  buttons.forEach((b, i) => {
    let d = b.getBoundingClientRect();
    if (d.right > 0 && d.right > 0) {
      let t = document.createElement('div');
      var lent = Math.floor((i / chars.length) + 1);
      var word = ''
      while (lent > 0) {
        let ch = chars[i % chars.length];
        word = `${word}${ch}`;
        lent = lent - 1;
      }
      t.textContent = word.slice(-1);
      t.className = 'b-1 br2 p1 s0 bold fixed gaze z2'
      t.style = `top: ${Math.max(0, d.top - 10)}px; left: ${Math.max(0, d.left - 10)}px;`
      document.getElementById('eye')?.parentNode.appendChild(t);
      window.eye.spots = [[word, b], ...(window.eye?.spots || [])]
    }
  })
}
window.addEventListener('keydown', handleKey);
