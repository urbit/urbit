import React, { Component } from 'react';
import { SidebarSwitcher } from '../../../../components/SidebarSwitch';
import { Spinner } from '../../../../components/Spinner';
import { Link } from 'react-router-dom';
import { Controlled as CodeMirror } from 'react-codemirror2';
import { dateToDa } from '../../../../lib/util';

import 'codemirror/mode/markdown/markdown';

export class EditPost extends Component {
  constructor(props) {
    super(props);
    this.state = {
      body: '',
      submit: false,
      awaiting: false
    };
    this.postSubmit = this.postSubmit.bind(this);
    this.bodyChange = this.bodyChange.bind(this);
  }

  componentDidMount() {
    this.componentDidUpdate();
  }

  componentDidUpdate(prevProps) {
    const { props, state } = this;
    const contents = props.notebooks[props.ship]?.[props.book]?.notes?.[props.note]?.file;
    if (prevProps && prevProps.api !== props.api) {
      if (!contents) {
        props.api?.fetchNote(props.ship, props.book, props.note);
      }
    }
    if (contents && state.body === '') {
      const notebook = props.notebooks[props.ship][props.book];
      const note = notebook.notes[props.note];
      const file = note.file;
      const body = file.slice(file.indexOf(';>') + 3);
      this.setState({ body: body });
    }
  }

  postSubmit() {
    const { props, state } = this;
    const notebook = props.notebooks[props.ship][props.book];
    const note = notebook.notes[props.note];
    const title = note.title;
    const editNote = {
      'edit-note': {
        who: props.ship.slice(1),
        book: props.book,
        note: props.note,
        title: title,
        body: state.body
      }
    };
    this.setState({ awaiting: true });
    this.props.api.publish.publishAction(editNote).then(() => {
      const editIndex = props.location.pathname.indexOf('/edit');
      const noteHref = props.location.pathname.slice(0, editIndex);
      this.setState({ awaiting: false });
      props.history.push(noteHref);
    });
  }

  bodyChange(editor, data, value) {
    const submit = !(value === '');
    this.setState({ body: value, submit: submit });
  }

  render() {
    const { props, state } = this;
    const notebook = props.notebooks[props.ship][props.book];
    const note = notebook.notes[props.note];
    const title = note.title;
    let date = dateToDa(new Date(note['date-created']));
    date = date.slice(1, -10);

    const submitStyle = (state.submit)
      ? { color: '#2AA779', cursor: 'pointer' }
      : { color: '#B1B2B3', cursor: 'auto' };

    const hrefIndex = props.location.pathname.indexOf('/note/');
    const publishsubStr = props.location.pathname.substr(hrefIndex);
    const popoutHref = `/~publish/popout${publishsubStr}`;

    const hiddenOnPopout = (props.popout)
      ? '' : 'dib-m dib-l dib-xl';

    const options = {
      mode: 'markdown',
      theme: 'tlon',
      lineNumbers: false,
      lineWrapping: true,
      scrollbarStyle: null,
      cursorHeight: 0.85
    };

    return (
      <div className="f9 h-100 relative publish">
        <div className="w-100 tl pv4 flex justify-center">
          <SidebarSwitcher
            sidebarShown={props.sidebarShown}
            popout={props.popout}
            api={this.props.api}
          />
          <button
            className="v-mid bg-transparent w-100 w-80-m w-90-l mw6 tl h1 pl4"
            disabled={!state.submit}
            style={submitStyle}
            onClick={this.postSubmit}
          >
            Save "{title}"
          </button>
        <Link
          className={'dn absolute right-1 top-1 ' + hiddenOnPopout}
          to={popoutHref}
          target="_blank"
        >
          <img src="/~landscape/img/popout.png"
            height={16}
            width={16}
          />
        </Link>
        </div>
        <div className="mw6 center">
        <div className="pl4">
          <div className="gray2">{date}</div>
        </div>
        <div className="EditPost">
          <CodeMirror
            value={state.body}
            options={options}
            onBeforeChange={(e, d, v) => this.bodyChange(e, d, v)}
            onChange={(editor, data, value) => {}}
          />
            <Spinner text="Editing post..." awaiting={this.state.awaiting} classes="absolute bottom-1 right-1 ba b--gray1-d pa2" />
        </div>
        </div>
      </div>
    );
  }
}

export default EditPost;
