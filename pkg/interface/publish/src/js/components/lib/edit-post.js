import React, { Component } from 'react';
import { SidebarSwitcher } from './icons/icon-sidebar-switch';
import { Route, Link } from 'react-router-dom';
import { Controlled as CodeMirror } from 'react-codemirror2';
import { dateToDa } from '/lib/util';

import 'codemirror/mode/markdown/markdown';

export class EditPost extends Component {
  constructor(props) {
    super(props);
    this.state = {
      body: '',
      submit: false
    }
    this.postSubmit = this.postSubmit.bind(this);
    this.bodyChange = this.bodyChange.bind(this);
  }

  componentDidMount() {
    const { props } = this;
    if (!(props.notebooks[props.ship]) ||
      !(props.notebooks[props.ship][props.book]) ||
      !(props.notebooks[props.ship][props.book].notes[props.note]) ||
      !(props.notebooks[props.ship][props.book].notes[props.note].file)) {
      window.api.fetchNote(props.ship, props.book, props.note);
    }
    else {
      let notebook = props.notebooks[props.ship][props.book];
      let note = notebook.notes[props.note];
      let file = note.file;
      let body = file.slice(file.indexOf(';>') + 2);
      this.setState({body: body});
    }
  }

  postSubmit() {
    let { props, state } = this;
    let notebook = props.notebooks[props.ship][props.book];
    let note = notebook.notes[props.note];
    let title = note.title;
    let editNote = {
      "edit-note": {
        who: props.ship.slice(1),
        book: props.book,
        note: props.note,
        title: title,
        body: state.body
      }
    }
    window.api.setSpinner(true);
    window.api.action("publish", "publish-action", editNote).then(() => {
      let editIndex = props.location.pathname.indexOf("/edit");
      let noteHref = props.location.pathname.slice(0, editIndex);
      window.api.setSpinner(false);
      props.history.push(noteHref);
    });
  }

  bodyChange(editor, data, value) {
    let submit = !(value === '');
    this.setState({ body: value, submit: submit });
  }

  render() {
    const { props, state } = this;
    let notebook = props.notebooks[props.ship][props.book];
    let note = notebook.notes[props.note];
    let title = note.title;
    let date = dateToDa(new Date(note["date-created"]));
    date = date.slice(1, -10);

    let submitStyle = (state.submit)
      ? { color: '#2AA779', cursor: "pointer" }
      : { color: '#B1B2B3', cursor: "auto" };

    let hrefIndex = props.location.pathname.indexOf("/note/");
    let publishsubStr = props.location.pathname.substr(hrefIndex)
    let popoutHref = `/~publish/popout${publishsubStr}`;

    let hiddenOnPopout = (props.popout)
      ? "" : "dib-m dib-l dib-xl";

    const options = {
      mode: 'markdown',
      theme: 'tlon',
      lineNumbers: false,
      lineWrapping: true,
      scrollbarStyle: null,
      cursorHeight: 0.85
    };

    return (
      <div className="f9 h-100 relative">
        <div className="w-100 tl pv4 flex justify-center">
          <SidebarSwitcher
            sidebarShown={props.sidebarShown}
            popout={props.popout}
          />
          <button
            className="v-mid w-100 mw6 tl h1 pl4"
            disabled={!state.submit}
            style={submitStyle}
            onClick={this.postSubmit}>
            Save "{title}"
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
        <div className="overflow-container mw6 center">
        <div className="pl4">
          <div className="gray2">{date}</div>
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
    )
  }
}

export default EditPost;