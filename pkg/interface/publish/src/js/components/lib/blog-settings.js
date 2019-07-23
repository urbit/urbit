import React, { Component } from 'react';
import classnames from 'classnames';

class SaveLink extends Component {
  constructor(props) {
    super(props);
  }

  render() {
    if (this.props.enabled) {
      return (
        <button className="label-regular b"
          onClick={this.props.action}>
          -> Save
        </button>
      );
    } else {
      return (
        <p className="label-regular b gray-50">
          -> Save
        </p>
      );
    }
  }
}

export class BlogSettings extends Component {
  constructor(props) {
    super(props);

    this.state = {
      title: '',
      awaitingTitleChange: false,
    }

    this.rename = this.rename.bind(this);
    this.titleChange = this.titleChange.bind(this);
    this.deleteBlog = this.deleteBlog.bind(this);
  }

  rename() {
    let edit = {
      "edit-collection": {
        name: this.props.blogId,
        title: this.state.title,
      }
    }
    this.setState({
      awaitingTitleChange: true,
    }, () => {
      this.props.api.action("publish", "publish-action", edit);
    });
  }

  titleChange(evt) {
    this.setState({title: evt.target.value});
  }

  deleteBlog() {
    let del = {
      "delete-collection": {
        coll: this.props.blogId,
      }
    }
    this.props.api.action("publish", "publish-action", del);
    this.props.history.push("/~publish/recent");
  }

  componentDidUpdate(prevProps) {
    if (this.state.awaitingTitleChange) {
      if (prevProps.title !== this.props.title){
        this.titleInput.value = '';
        this.setState({
          awaitingTitleChange: false,
        });
      }
    }
  }

  render() {
    let back = '<- Back to notes'
    let enableSave = ((this.state.title !== '') && 
      (this.state.title !== this.props.title) &&
      !this.state.awaitingTitleChange);
    return (
      <div className="flex-col mw-688" style={{marginTop:48}}>
        <hr className="gray-30" style={{marginBottom:25}}/>
        <p className="label-regular pointer b" onClick={this.props.back}>
          {back}
        </p>
        <p className="body-large b" style={{marginTop:16, marginBottom: 20}}>
          Settings
        </p>
        <div className="flex">
          <div className="flex-col w-100">
            <p className="body-regular-400">Delete Notebook</p> 
            <p className="gray-50 label-small-2" style={{marginTop:12, marginBottom:8}}>
              Permanently delete this notebook
            </p>
            <button className="red label-regular b" onClick={this.deleteBlog}>
              -> Delete
            </button>
          </div>
          <div className="flex-col w-100">
            <p className="body-regular-400">Rename</p> 
            <p className="gray-50 label-small-2" style={{marginTop:12, marginBottom:23}}>
              Change the name of this notebook
            </p>
            <p className="label-small-2">Notebook Name</p>
            <input className="body-regular-400 w-100"
              ref={(el) => {this.titleInput = el}}
              style={{marginBottom:8}}
              placeholder={this.props.title}
              onChange={this.titleChange}
              disabled={this.state.awaitingTitleChange}/>
            <SaveLink action={this.rename} enabled={enableSave}/>
          </div>
        </div>
      </div>
    );
  }
}
