import React, { Component } from 'react'
import classnames from 'classnames';
import { Route, Link } from 'react-router-dom';
import urbitOb from 'urbit-ob';

//TODO textarea + join button to make an api call
export class JoinScreen extends Component {
  constructor(props) {
    super(props);

    this.state = {
      book: '/',
      error: false
    };

    this.bookChange = this.bookChange.bind(this);
  }

  notebooksInclude(text, notebookObj) {
    let verdict = false;
    let keyPair = [];
    // validate that it's a worthwhile thing to check
    // certainly a unit would be nice here
    if (text.indexOf('/') === -1) {
      return verdict;
    } else {
      keyPair = text.split('/');
    };
    // check both levels of object
    if (keyPair[0] in notebookObj) {
      if (keyPair[1] in notebookObj[keyPair[0]]) {
        verdict = true;
      }
    }
    return verdict;
  }

  onClickJoin() {
    const { props, state } = this;

    let text = state.book;
    // an error condition to prevent double joins?
    if (this.notebooksInclude(state.book,props.notebooks) ||
        text.length === 0) {
      props.history.push('/~publish');
    }

    let book = text.split('/');
    let ship = book[0];
    book.splice(0, 1);
    book = '/' + book.join('/');

    if (book.length < 2 || !urbitOb.isValidPatp(ship)) {
      this.setState({
        error: true,
      });
      return;
    }

    let actionData = {
      subscribe: {
        who: ship.replace('~',''),
        book: /\/?(.*)/.exec(book)[1]
      }
    }

    // TODO: askHistory setting
    window.api.action("publish","publish-action", actionData);

  }

  bookChange(event) {
    this.setState({
      book: event.target.value
    });
  }

  render() {
    const { props } = this;

    let joinClasses = "db f9 green2 ba pa2 b--green2 bg-gray0-d pointer";
    if ((!this.state.book) || (this.state.book === "/")) {
      joinClasses = 'db f9 gray2 ba pa2 b--gray3 bg-gray0-d pointer';
    }

    let errElem = (<span />);
    if (this.state.error) {
      errElem = (
        <span className="f9 inter red2 db">
          Notebook must have a valid name.
        </span>
      );
    }

    return (
      <div className={"h-100 w-100 pt2 overflow-x-hidden flex flex-column " +
      "bg-gray0-d white-d pa3"}>
        <div
          className="w-100 dn-m dn-l dn-xl inter pt1 pb6 f8">
          <Link to="/~chat/">{"⟵ All Notebooks"}</Link>
        </div>
        <h2 className="mb3 f8">Subscribe to an Existing Notebook</h2>
        <div className="w-100">
          <p className="f8 lh-copy mt3 db">Enter a <span className="mono">~ship/notebook-name</span></p>
          <p className="f9 gray2 mb4">Notebook names use lowercase, hyphens, and slashes.</p>
          <textarea
            ref={ e => { this.textarea = e; } }
            className={"f7 mono ba bg-gray0-d white-d pa3 mb2 db " +
            "focus-b--black b--gray3 b--gray2-d "}
            placeholder="~zod/dream-journal"
            spellCheck="false"
            rows={1}
            onKeyPress={e => {
              if (e.key === "Enter") {
                e.preventDefault();
                this.onClickJoin();
              }
            }}
            style={{
              resize: 'none',
            }}
            onChange={this.bookChange} />
          {errElem}
          <br />
          <button
            onClick={this.onClickJoin.bind(this)}
            className={joinClasses}
          >Join Chat</button>
        </div>
      </div>
    );
  }
}

export default JoinScreen
