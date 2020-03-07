import React, { Component } from 'react';

export class Settings extends Component {
  constructor(props){
    super(props);
    this.state = {
      title: "",
      description: "",
    }
    this.deleteNotebook = this.deleteNotebook.bind(this);
    this.changeTitle = this.changeTitle.bind(this);
    this.changeDescription = this.changeDescription.bind(this);
  }

  componentDidMount() {
    const { props } = this;
    if (props.notebook) {
      this.setState({
        title: props.notebook.title,
        description: props.notebook.about
      })
    }
  }

  componentDidUpdate(prevProps) {
    const { props } = this;
    if (prevProps !== this.props) {
      if (props.notebook) {
        this.setState({
          title: props.notebook.title,
          description: props.notebook.about
        })
      }
    }
  }

  changeTitle(event) {
    this.setState({title: event.target.value});
  }

  changeDescription(event) {
    this.setState({description: event.target.value});
  }

  deleteNotebook(){
    let action = {
      "del-book": {
        book: this.props.book
      }
    }
    window.api.setSpinner(true);
    window.api.action("publish", "publish-action", action).then(() => {
      window.api.setSpinner(false);
      this.props.history.push("/~publish");
    });
  }

  render() {
    if (this.props.host.slice(1) === window.ship) {
      return (
        <div className="flex-column">
          <p className="f9 mt3 lh-copy db">Delete Notebook</p>
          <p className="f9 gray2 db mb4">
            Permanently delete this notebook. (All current members will no
            longer see this notebook)
          </p>
          <button
            className="bg-transparent b--red2 red2 pointer dib f9 ba pa2"
            onClick={this.deleteNotebook}>
            Delete this notebook
          </button>
          <p className="f8 mt3 lh-copy">Rename</p>
          <p className="f9 gray2 db mb4">Change the name of this notebook</p>
          <div className="relative w-100 flex" style={{ maxWidth: "29rem" }}>
            <input
              className={
                "f8 ba b--gray3 b--gray2-d bg-gray0-d white-d " +
                "focus-b--black focus-b--white-d pa3 db w-100 flex-auto mr3"
              }
              value={this.state.title}
              onChange={this.changeTitle}
            />
            <span
              className="f8 absolute pa3 inter pointer"
              style={{ right: 12, top: 1 }}
              ref="rename"
              onClick={() => {
                  window.api.setSpinner(true);
                  window.api
                    .action("publish", "publish-action", {
                      //TODO
                    })
                    .then(() => {
                      this.refs.rename.innerText = "Saved";
                      window.api.setSpinner(false);
                    });
              }}>
              Save
            </span>
          </div>
          <p className="f8 mt3 lh-copy">Change description</p>
          <p className="f9 gray2 db mb4">Change the description of this notebook</p>
          <div className="relative w-100 flex" style={{ maxWidth: "29rem" }}>
            <input
              className={
                "f8 ba b--gray3 b--gray2-d bg-gray0-d white-d " +
                "focus-b--black focus-b--white-d pa3 db w-100 flex-auto mr3"
              }
              value={this.state.description}
              onChange={this.changeDescription}
            />
            <span
              className="f8 absolute pa3 inter pointer"
              style={{ right: 12, top: 1 }}
              ref="description"
              onClick={() => {
                  window.api.setSpinner(true);
                  window.api
                    .action("publish", "publish-action", {
                      //TODO
                    })
                    .then(() => {
                      this.refs.description.innerText = "Saved";
                      window.api.setSpinner(false);
                    });
              }}>
              Save
            </span>
          </div>
        </div>
      );
    } else {
      return null;
    }
  }
}

export default Settings
