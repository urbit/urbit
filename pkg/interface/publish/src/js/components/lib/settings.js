import React, { Component } from 'react';

export class Settings extends Component {
  constructor(props){
    super(props)
    this.deleteNotebook = this.deleteNotebook.bind(this);
  }

  deleteNotebook(){
    let action = {
      "del-book": {
        book: this.props.book
      }
    }
    window.api.action("publish", "publish-action", action);
    this.props.history.push('/~publish');
  }

  render() {
    if (this.props.host.slice(1) === window.ship) {
      return (
        <div className="flex-column">
          <p className="f9 mt3 lh-copy db">Delete Notebook</p>
          <p className="f9 gray2 db mb4">
            Permanently delete this notebook. (All current members will no longer see this notebook)
          </p>
          <button className="b--red2 red2 pointer dib f9 ba pa2"
            onClick={this.deleteNotebook}>
            Delete this notebook
          </button>
        </div>

      )
    } else {
      return null;
    }
  }
}

export default Settings
