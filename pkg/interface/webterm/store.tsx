import { saveAs } from 'file-saver';
import bel from './lib/bel';

export default class Store {
  state: any;
  api: any;
  setState: any;
  constructor() {
    this.state = this.initialState();
  }

  initialState() {
    return {
      lines: [''],
      cursor: 0
    };
  }

  clear() {
    this.setState(this.initialState());
  }

  handleEvent(data) {
    //  process slogs
    //
    if (data.slog) {
      this.state.lines.splice(this.state.lines.length-1, 0, { lin: [data.slog] });
      this.setState({ lines: this.state.lines });
      return;
    }

    //  process blits
    //
    const blit = data.data;
    switch (Object.keys(blit)[0]) {
      case 'bel':
        bel.play();
        break;
      case 'clr':
        this.state.lines = this.state.lines.slice(-1);
        this.setState({ lines: this.state.lines });
        break;
      case 'hop':
        //  since lines are lists of characters that might span multiple
        //  codepoints, we need to calculate the byte-wise cursor position
        //  to avoid incorrect cursor rendering.
        //
        const line = this.state.lines[this.state.lines.length - 1];
        let hops;
        if (line.lin) {
          hops = line.lin.slice(0, blit.hop);
        } else if (line.klr) {
          hops = line.klr.reduce((h, p) => {
            if (h.length >= blit.hop)
return h;
            return [...h, ...p.text.slice(0, blit.hop - h.length)];
          }, []);
        }
        this.setState({ cursor: hops.join('').length });
        break;
      case 'lin':
        this.state.lines[this.state.lines.length - 1] = blit;
        this.setState({ lines: this.state.lines });
        break;
      case 'klr':
        this.state.lines[this.state.lines.length - 1] = blit;
        this.setState({ lines: this.state.lines });
        break;
      case 'mor':
        this.state.lines.push('');
        this.setState({ lines: this.state.lines });
        break;
      case 'sag':
        blit.sav = blit.sag;
        break;
      case 'sav':
        const name = blit.sav.path.split('/').slice(-2).join('.');
        const buff = new Buffer(blit.sav.file, 'base64');
        const blob = new Blob([buff], { type: 'application/octet-stream' });
        saveAs(blob, name);
        break;
      case 'url':
        // TODO  too invasive? just print as <a>?
        window.open(blit.url);
        break;
      default: console.log('weird blit', blit);
    }
  }

  setStateHandler(setState) {
    this.setState = setState;
  }
}

