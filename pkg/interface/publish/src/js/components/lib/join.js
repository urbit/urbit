import React, { Component } from 'react'
import classnames from 'classnames';
import { Route, Link } from 'react-router-dom';
import { Spinner } from './icons/icon-spinner';
import urbitOb from 'urbit-ob';

export class JoinScreen extends Component {
  constructor(props) {
    super(props);

    this.state = {
      book: '',
      error: false,
      awaiting: null,
      disable: false
    };

    this.bookChange = this.bookChange.bind(this);
  }

  componentDidMount() {
    // direct join from incoming URL
    if ((this.props.ship) && (this.props.notebook)) {
      let incomingBook = `${this.props.ship}/${this.props.notebook}`;
      this.setState({book: incomingBook}, () => {
        this.onClickJoin();
      })
    }
  }

  componentDidUpdate() {
    // redirect to notebook when we have it
    if (this.props.notebooks) {
      if (this.state.awaiting) {
        let book = this.state.awaiting.split("/");
        let ship = book[0];
        let notebook = book[1];
        if ((ship in this.props.notebooks) &&
        (notebook in this.props.notebooks[ship])) {
          this.setState({disable: false, book: "/"});
          this.props.history.push(`/~publish/notebook/${ship}/${notebook}`)
        }
      }
    }
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

    let book = text.split('/');
    let ship = book[0];
    book.splice(0, 1);
    book = '/' + book.join('/');

    if (this.notebooksInclude(state.book, props.notebooks)) {
      let href = `/~publish/notebook/${ship}${book}`
      return props.history.push(href);
    }

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
    this.setState({disable: true});
    window.api.action("publish","publish-action", actionData).catch((err) => {
      console.log(err)
    }).then(() => {
      this.setState({awaiting: text})
    });

  }

  bookChange(event) {
    this.setState({
      book: event.target.value
    });
  }

  render() {
    const { props, state } = this;

    let joinClasses = "db f9 green2 ba pa2 b--green2 bg-gray0-d pointer";
    if ((state.disable) || (!state.book) || (state.book === "/")) {
      joinClasses = 'db f9 gray2 ba pa2 b--gray3 bg-gray0-d';
    }

    let errElem = (<span />);
    if (state.error) {
      errElem = (
        <span className="f9 inter red2 db">
          Notebook must have a valid name.
        </span>
      );
    }

    return (
      <div className={"h-100 w-100 pt4 overflow-x-hidden flex flex-column " +
      "bg-gray0-d white-d pa3"}>
        <div
          className="w-100 dn-m dn-l dn-xl inter pt1 pb6 f8">
          <Link to="/~publish/">{"⟵ All Notebooks"}</Link>
        </div>
        <h2 className="mb3 f8">Subscribe to an Existing Notebook</h2>
        <div className="w-100">
          <p className="f8 lh-copy mt3 db">Enter a <span className="mono">~ship/notebook-name</span></p>
          <p className="f9 gray2 mb4">Notebook names use lowercase, hyphens, and slashes.</p>
          <textarea
            ref={ e => { this.textarea = e; } }
            className={"f7 mono ba bg-gray0-d white-d pa3 mb2 db " +
            "focus-b--black focus-b--white-d b--gray3 b--gray2-d nowrap "}
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
            onChange={this.bookChange}
            value={this.state.book}
            />
          {errElem}
          <br />
          <button
            disabled={(this.state.disable) || (!state.book) || (state.book === "/")}
            onClick={this.onClickJoin.bind(this)}
            className={joinClasses}
          >Join Notebook</button>
          <Spinner awaiting={this.state.disable} classes="mt4" text="Joining notebook..." />
        </div>
      </div>
    );
  }
}

export default JoinScreen;