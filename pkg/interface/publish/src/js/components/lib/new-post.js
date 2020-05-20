import React, { Component } from 'react'
import { SidebarSwitcher } from './icons/icon-sidebar-switch';
import { Spinner } from './icons/icon-spinner';
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
      disabled: false
    }

    this.postSubmit = this.postSubmit.bind(this);
    this.titleChange = this.titleChange.bind(this);
    this.bodyChange = this.bodyChange.bind(this);
  }

  postSubmit() {
    const { state, props } = this;
    if (state.submit && !state.disabled) {
      let newNote = {
        "new-note": {
          who: this.props.ship.slice(1),
          book: this.props.book,
          note: stringToSymbol(this.state.title),
          title: this.state.title,
          body: this.state.body,
        }
      }

      this.setState({ disabled: true });
      window.api.action("publish", "publish-action", newNote).then(() => {
        this.setState({ awaiting: newNote['new-note'].note });
      }).catch((err) => {
        if (err.includes("note already exists")) {
          let timestamp = Math.floor(Date.now() / 1000);
          newNote["new-note"].note += "-" + timestamp;
          this.setState({ awaiting: newNote['new-note'].note });
          window.api.action("publish", "publish-action", newNote);
        } else {
          this.setState({ disabled: false, awaiting: null })
        }
      });
    }
  }

  componentWillMount() {
    window.api.fetchNotebook(this.props.ship, this.props.book);
  }

  componentDidUpdate(prevProps, prevState) {
    let notebook = this.props.notebooks[this.props.ship][this.props.book];
    if (notebook.notes[this.state.awaiting]) {
      this.setState({ disabled: false, awaiting: null });
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

    let submitStyle = ((!state.disabled && state.submit) && (state.awaiting === null))
      ? { color: '#2AA779', cursor: "pointer" }
      : { color: '#B1B2B3', cursor: "auto" };

    let hrefIndex = props.location.pathname.indexOf("/notebook/");
    let publishsubStr = props.location.pathname.substr(hrefIndex)
    let popoutHref = `/~publish/popout${publishsubStr}`;

    let hiddenOnPopout = (props.popout)
      ? "" : "dib-m dib-l dib-xl";

    let newIndex = props.location.pathname.indexOf("/new");
    let backHref = props.location.pathname.slice(0, newIndex);
    return (
      <div className="f9 h-100 relative">
        <div className="w-100 dn-m dn-l dn-xl inter pt4 pb4 f9 pl4">
          <Link to={backHref}>{"<- Back"}</Link>
        </div>
        <div className="w-100 tl pv4 flex justify-center">
          <SidebarSwitcher
            sidebarShown={props.sidebarShown}
            popout={props.popout}
          />
          <button
            className={"bg-transparent v-mid w-100 w-90-l w-80-m mw6 tl h1 pl4"}
            disabled={(!state.submit && state.disabled) || (state.awaiting !== null)}
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
        <div className="mw6 center">
          <div className="pa4">
            <input
              autoFocus
              type="text"
              className="bg-transparent white-d w-100 pb2"
              onChange={this.titleChange}
              placeholder="New Post"
            />

            <div className="gray2">{date}</div>
          </div>

          <div className="NewPost">
            <CodeMirror
              value={state.body}
              options={options}
              onBeforeChange={(e, d, v) => this.bodyChange(e, d, v)}
              onChange={(editor, data, value) => {}}
            />
            <Spinner text="Creating post..." awaiting={this.state.disabled} classes="absolute bottom-1 right-1 ba b--gray1-d pa2" />
          </div>
        </div>
      </div>
    );
  }
}

export default NewPost;
