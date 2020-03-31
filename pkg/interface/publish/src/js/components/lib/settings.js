import React, { Component } from 'react';
import { writeText } from '../../lib/util';
import { Spinner } from './icons/icon-spinner';

export class Settings extends Component {
  constructor(props){
    super(props);
    this.state = {
      title: "",
      description: "",
      comments: false,
      disabled: false,
      type: "Editing"
    }
    this.deleteNotebook = this.deleteNotebook.bind(this);
    this.changeTitle = this.changeTitle.bind(this);
    this.changeDescription = this.changeDescription.bind(this);
    this.changeComments = this.changeComments.bind(this);
  }

  componentDidMount() {
    const { props } = this;
    if (props.notebook) {
      this.setState({
        title: props.notebook.title,
        description: props.notebook.about,
        comments: props.notebook.comments
      })
    }
  }

  componentDidUpdate(prevProps) {
    const { props } = this;
    if (prevProps !== props) {
      if (props.notebook) {
        if (prevProps.notebook && prevProps.notebook !== props.notebook) {
          if (prevProps.notebook.title !== props.notebook.title) {
            this.setState({title: props.notebook.title});
          }
          if (prevProps.notebook.about !== props.notebook.about) {
            this.setState({description: props.notebook.about});
          }
          if (prevProps.notebook.comments !== props.notebook.comments) {
            this.setState({comments: props.notebook.comments})
          }
        }
      }
    }
  }

  changeTitle(event) {
    this.setState({title: event.target.value});
  }

  changeDescription(event) {
    this.setState({description: event.target.value});
  }

  changeComments() {
    this.setState({comments: !this.state.comments, disabled: true}, (() => {
      window.api.action("publish", "publish-action", {
        "edit-book": {
          book: this.props.book,
          title: this.props.notebook.title,
          about: this.props.notebook.about,
          coms: this.state.comments,
          group: null
        }
      }).then(() => {
        this.setState({disabled: false});
      })
    }));
  }

  deleteNotebook(){
    let action = {
      "del-book": {
        book: this.props.book
      }
    }
    this.setState({ disabled: true, type: "Deleting" });
    window.api.action("publish", "publish-action", action).then(() => {
      this.props.history.push("/~publish");
    });
  }

  render() {
    let commentsSwitchClasses = (this.state.comments)
      ? "relative checked bg-green2 br3 h1 toggle v-mid z-0"
      : "relative bg-gray4 bg-gray1-d br3 h1 toggle v-mid z-0";

    let copyShortcode = <div>
      <p className="f9 mt3 lh-copy">Share</p>
      <p className="f9 gray2 mb4">Share a shortcode to join this notebook</p>
      <div className="relative w-100 flex" style={{ maxWidth: "29rem" }}>
        <input
          className={"f8 mono ba b--gray3 b--gray2-d bg-gray0-d white-d " +
            "pa3 db w-100 flex-auto mr3"}
          disabled={true}
          value={`${this.props.host}/${this.props.book}` || ""}
        />
        <span className="f8 pointer absolute pa3 inter"
          style={{ right: 12, top: 1 }}
          ref="copy"
          onClick={() => {
            writeText(`${this.props.host}/${this.props.book}`);
            this.refs.copy.innerText = "Copied"
          }}>
            Copy
          </span>
      </div>
    </div>

    if (this.props.host.slice(1) === window.ship) {
      return (
        <div className="flex-column">
          {copyShortcode}
          <p className="f9 mt6 lh-copy db">Delete Notebook</p>
          <p className="f9 gray2 db mb4">
            Permanently delete this notebook. (All current members will no
            longer see this notebook)
          </p>
          <button
            className="bg-transparent b--red2 red2 pointer dib f9 ba pa2"
            onClick={this.deleteNotebook}>
            Delete this notebook
          </button>
          <p className="f9 mt6 lh-copy">Rename</p>
          <p className="f9 gray2 db mb4">Change the name of this notebook</p>
          <div className="relative w-100 flex" style={{ maxWidth: "29rem" }}>
            <input
              className={
                "f8 ba b--gray3 b--gray2-d bg-gray0-d white-d " +
                "focus-b--black focus-b--white-d pa3 db w-100 flex-auto mr3"
              }
              value={this.state.title}
              onChange={this.changeTitle}
              disabled={this.state.disabled}
              onBlur={() => {
                this.setState({ disabled: true });
                window.api
                  .action("publish", "publish-action", {
                    "edit-book": {
                      book: this.props.book,
                      title: this.state.title,
                      about: this.props.notebook.about,
                      coms: this.props.notebook.comments,
                      group: null
                    }
                  })
                  .then(() => {
                    this.setState({ disabled: false })
                  });
              }}
            />
          </div>
          <p className="f9 mt6 lh-copy">Change description</p>
          <p className="f9 gray2 db mb4">Change the description of this notebook</p>
          <div className="relative w-100 flex" style={{ maxWidth: "29rem" }}>
            <input
              className={
                "f8 ba b--gray3 b--gray2-d bg-gray0-d white-d " +
                "focus-b--black focus-b--white-d pa3 db w-100 flex-auto mr3"
              }
              value={this.state.description}
              onChange={this.changeDescription}
              onBlur={() => {
                this.setState({ disabled: true });
                window.api
                  .action("publish", "publish-action", {
                    "edit-book": {
                      book: this.props.book,
                      title: this.props.notebook.title,
                      about: this.state.description,
                      coms: this.props.notebook.comments,
                      group: null
                    }
                  })
                  .then(() => {
                    this.setState({ disabled: false });
                  });
              }}
            />
          </div>
          <div className="mv6">
            <input
              type="checkbox"
              style={{ WebkitAppearance: "none", width: 28 }}
              className={commentsSwitchClasses}
              onChange={this.changeComments}
            />
            <span className="dib f9 white-d inter ml3">Comments</span>
            <p className="f9 gray2 pt1" style={{ paddingLeft: 40 }}>
            Subscribers may comment when enabled
            </p>
          </div>
          <Spinner
            awaiting={this.state.disabled}
            classes="absolute right-1 bottom-1 pa2 ba b--black b--gray0-d white-d"
            text={`${this.state.type} notebook...`}
          />
        </div>
      );
    } else {
      return copyShortcode;
    }
  }
}

export default Settings
