import React, { Component } from 'react'
import { Controlled as CodeMirror } from 'react-codemirror2'
import { dateToDa, stringToSymbol } from '/lib/util';

import 'codemirror/mode/markdown/markdown';

export class NewPost extends Component {
  constructor(props) {
    super(props);
    this.state = {
      body: "# Hello",
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
        who: this.props.host.slice(1),
        book: this.props.notebookName,
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

  componentDidUpdate(prevProps, prevState) {
    if (this.props.notebook.notes[this.state.awaiting]) {
      let redirect =
     `/~publish/note/${this.props.host}/${this.props.notebookName}/${this.state.awaiting}`;
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
    const options = {
      mode: 'markdown',
      theme: 'tlon',
      lineNumbers: false,
      lineWrapping: true,
      scrollbarStyle: null,
      cursorHeight: 0.85
    };

    let date = dateToDa(new Date()).slice(1, -10);

    let submitStyle = (this.state.submit)
      ? { color: '#2AA779', cursor: "pointer" }
      : { color: '#B1B2B3', cursor: "auto" };

    return (
      <div className="center mw7 f9 h-100">
        <div style={{padding: 16}} className="flex-col">
          <div className="w-100 tl">
            <button disabled={!this.state.submit} style={submitStyle}
                onClick={this.postSubmit}>
              Publish To {this.props.notebook.title}
            </button>
          </div>

          <input type="text"
            style={{paddingBottom: 8, paddingTop: 24}}
            className="w-100"
            onChange={this.titleChange}
            placeholder="New Post" />

          <div style={{color:'#7F7F7F'}}>{date}</div>
        </div>

        <div className="NewPost">
          <CodeMirror
            value={this.state.body}
            options={options}
            onBeforeChange={(e, d, v) => this.bodyChange(e, d, v)}
            onChange={(editor, data, value) => {}}
          />
        </div>
      </div>
    )
  }
}

export default NewPost;
