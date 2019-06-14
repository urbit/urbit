import React, { Component } from 'react';
import classnames from 'classnames';
import { Link } from 'react-router-dom';
import _ from 'lodash';

class SideTab extends Component {
  constructor(props) {
    super(props)
  }

  render(props) {
    if (this.props.enabled){
      return (
        <div className="w1 z-2"
          style={{
            flexGrow:1,
        }}>
          <p className="pointer" onClick={this.props.postSubmit}>
            -> Post
          </p>
          <p>Discard post</p>
        </div>
      );
    }
    return null;
  }
}
 
export class NewPost extends Component {
  constructor(props){
    super(props);

    this.state = {
      title: "",
      body: "",
      awaiting: false,
    };

    this.titleChange = this.titleChange.bind(this);
    this.bodyChange = this.bodyChange.bind(this);
    this.postSubmit = this.postSubmit.bind(this);
  }

  titleChange(evt){
    this.setState({title: evt.target.value});
  }

  bodyChange(evt){
    this.setState({body: evt.target.value});
  }

  stringToSymbol(str){
    let result = '';
    for (var i=0; i<str.length; i++){
      var n = str.charCodeAt(i);
      if (( (n >= 97) && (n <= 122) ) ||
          ( (n >= 48) && (n <= 57) ))
      {
        result += str[i];
      } else if ( (n >= 65) &&  (n <= 90) ) 
      {
        result += String.fromCharCode(n + 32);
      } else {
        result += '-';
      }
    }
    return result;
  }

  postSubmit() {
    let last = _.get(this.props, 'location.state', false);
    let ship = window.ship;
    let blogId = null;

    if (last){
      ship = last.lastParams.ship.slice(1); 
      blogId = last.lastParams.blog;
    }

    let postTitle = this.state.title;
    let postId = this.stringToSymbol(postTitle);
    let permissions = {
      read: {
        mod: 'black',
        who: [],
      },
      write: {
        mod: 'white',
        who: [],
      }
    }
    let content = this.state.body;

    let data = {
      "new-post" : {
        who: ship,
        coll: blogId,
        name: postId,
        title: postTitle,
        comments: "open",
        perm: permissions,
        content: content,
      },
    };

    this.setState({
      awaiting: {
        ship: ship,
        blogId: blogId,
        postId: postId,
      }
    });

    this.props.api.action("write", "write-action", data);
  }

  componentDidUpdate(prevProps, prevState) {
    if (this.state.awaiting) {
      let ship = this.state.awaiting.ship;
      let blogId = this.state.awaiting.blogId;
      let postId = this.state.awaiting.postId;

      if (this.state.awaiting.ship == window.ship) {

        let post = _.get(this.props, `pubs[${blogId}].posts[${postId}].post`, false);
        let comments = _.get(this.props, `pubs[${blogId}].posts[${postId}].comments`, false);
        if (post && comments) {
          let redirect = `/~publish/~${ship}/${blogId}/${postId}`;
          this.props.history.push(redirect);
        }

      } else {

        let post = _.get(this.props, `subs[${ship}][${blogId}].posts[${postId}].post`, false);
        let comments = _.get(this.props, `subs[${ship}][${blogId}].posts[${postId}].comments`, false);
        if (post && comments) {
          let redirect = `/~publish/~${ship}/${blogId}/${postId}`;
          this.props.history.push(redirect);
        }

      }
    }
  }

  render() {
    let enabledTab = ((this.state.title !== "") && (this.state.body !== ""));

    return (
      <div className="w-100 relative" style={{height: 2000, top: 'calc(50% - 124px)'}}>

        <div className="flex w-100 z-2" style={{position: "sticky", top:0}}> 
          <div className="w1 z-0" style={{flexGrow:1}}>
          </div>
          <div className="mw-688 w-100 z-0">
          </div>
          <SideTab enabled={enabledTab} postSubmit={this.postSubmit} />
        </div>

        <div className="flex absolute w-100 z-0" style={{top:0}}>
          <div className="w1 z-0" style={{flexGrow:1}}>
          </div>
          <div className="flex-col w-100 mw-688 w-100 z-1">
            <input autoFocus
              className="header-2 w-100 b--none"
              type="text"
              name="postName"
              placeholder="Add a Title"
              onChange={this.titleChange}
            />
            <textarea className="body-regular-400 w-100 b--none"
              style={{resize:"none"}}
              type="text"
              name="postBody"
              placeholder="And type away."
              onChange={this.bodyChange}>
            </textarea>
          </div>
          <div className="w1 z-0" style={{flexGrow:1}}>
          </div>
        </div>
      </div>
    );
  }
}
