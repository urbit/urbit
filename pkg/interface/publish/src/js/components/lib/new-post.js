import React, { Component } from 'react'
import { SidebarSwitcher } from './icons/icon-sidebar-switch';
import { Route, Link } from 'react-router-dom';
import { Controlled as CodeMirror } from 'react-codemirror2'
import { dateToDa, stringToSymbol } from '/lib/util';

import 'codemirror/mode/markdown/markdown';

export class NewPost extends Component {
  constructor(props) {
    super(props);
    this.state = {
      body: '',
      title: '',
      submit: false,
      awaiting: null,
    }

    this.postSubmit = this.postSubmit.bind(this);
    this.titleChange = this.titleChange.bind(this);
    this.bodyChange = this.bodyChange.bind(this);
  }

  postSubmit() {
    let newNote = {
      "new-note": {
        who: this.props.ship.slice(1),
        book: this.props.book,
        note: stringToSymbol(this.state.title),
        title: this.state.title,
        body: this.state.body,
      }
    }

    this.setState({
      awaiting: newNote["new-note"].note
    }, () => {
      window.api.action("publish", "publish-action", newNote);
    });
  }

  componentWillMount() {
    window.api.fetchNotebook(this.props.ship, this.props.book);
  }

  componentDidUpdate(prevProps, prevState) {
    let notebook = this.props.notebooks[this.props.ship][this.props.book];
    if (notebook.notes[this.state.awaiting]) {
      let popout = (this.props.popout) ? "popout/" : "";
      let redirect =
     `/~publish/${popout}note/${this.props.ship}/${this.props.book}/${this.state.awaiting}`;
      this.props.history.push(redirect);
    }
  }

  titleChange(evt) {
    let submit = !(evt.target.value === '' || this.state.body === '');
    this.setState({title: evt.target.value, submit: submit});
  }

  bodyChange(editor, data, value) {
    let submit = !(value === '' || this.state.title === '');
    this.setState({body: value, submit: submit});
  }

  render() {
    const { props, state } = this;

    let notebook = props.notebooks[props.ship][props.book];

    const options = {
      mode: 'markdown',
      theme: 'tlon',
      lineNumbers: false,
      lineWrapping: true,
      scrollbarStyle: null,
      cursorHeight: 0.85
    };

    let date = dateToDa(new Date()).slice(1, -10);

    let submitStyle = (state.submit)
      ? { color: '#2AA779', cursor: "pointer" }
      : { color: '#B1B2B3', cursor: "auto" };

    let hrefIndex = props.location.pathname.indexOf("/notebook/");
    let publishsubStr = props.location.pathname.substr(hrefIndex)
    let popoutHref = `/~publish/popout${publishsubStr}`;

    let hiddenOnPopout = (props.popout)
      ? "" : "dib-m dib-l dib-xl";

    return (
      <div className="f9 h-100 relative">
        <div className="w-100 tl pv4 flex justify-center">
          <SidebarSwitcher
            sidebarShown={props.sidebarShown}
            popout={props.popout}
          />
          <button
            className={"v-mid w-100 mw7 tl h1 pl4"}
            disabled={!state.submit}
            style={submitStyle}
            onClick={this.postSubmit}>
            Publish To {notebook.title}
          </button>
          <Link
          className={"dn absolute right-1 top-1 " + hiddenOnPopout}
          to={popoutHref}
          target="_blank">
            <img src="/~publish/popout.png"
              height={16}
              width={16}
            />
          </Link>
        </div>
        <div className="overflow-container mw7 center">
          <div style={{ padding: 16 }}>
            <input
              autoFocus
              type="text"
              style={{ paddingBottom: 8 }}
              className="w-100"
              onChange={this.titleChange}
              placeholder="New Post"
            />

            <div style={{ color: "#7F7F7F" }}>{date}</div>
          </div>

          <div className="NewPost">
            <CodeMirror
              value={state.body}
              options={options}
              onBeforeChange={(e, d, v) => this.bodyChange(e, d, v)}
              onChange={(editor, data, value) => {}}
            />
          </div>
        </div>
      </div>
    );
  }
}

export default NewPost;
