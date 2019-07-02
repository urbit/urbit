import React, { Component } from 'react';
import classnames from 'classnames';
import { Link } from 'react-router-dom';
import _ from 'lodash';
import { PathControl } from '/components/lib/path-control';
import { withRouter } from 'react-router';
import { store } from '/store';

const PC = withRouter(PathControl);

class SideTab extends Component {
  constructor(props) {
    super(props);
  }

  render() {
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
      error: false,
    };

    this.titleChange = this.titleChange.bind(this);
    this.bodyChange = this.bodyChange.bind(this);
    this.postSubmit = this.postSubmit.bind(this);

    this.windowHeight = window.innerHeight - 48 - 76;

    this.bodyHeight = 54;
    this.titleHeight = 102;
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
    return result.replace(/^\-+|\-+$/g, '');
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
    };
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

    store.handleEvent({
      data: {
        spinner: true,
      }
    });

    this.props.api.action("write", "write-action", data);
  }

  componentDidUpdate(prevProps, prevState) {
    if (this.state.awaiting) {
      let ship = this.state.awaiting.ship;
      let blogId = this.state.awaiting.blogId;
      let postId = this.state.awaiting.postId;

      let post;
      let comments;

      if (this.state.awaiting.ship == window.ship) {
        post =
          _.get(this.props, `pubs[${blogId}].posts[${postId}].post`, false);
        comments =
          _.get(this.props, `pubs[${blogId}].posts[${postId}].comments`, false);
      } else {
        post =
          _.get(this.props, `subs[${ship}][${blogId}].posts[${postId}].post`, false);
        comments =
          _.get(this.props, `subs[${ship}][${blogId}].posts[${postId}].comments`, false);
      }

      if (post && comments) {
        store.handleEvent({
          data: {
            spinner: false,
          }
        });
        if (typeof(post) === 'String') {
          this.setState({
            error: post,
            awaiting: false,
          });
        } else {
          let redirect = `/~publish/~${ship}/${blogId}/${postId}`;
          this.props.history.push(redirect);
        }
      }
    }
  }

  titleChange(evt){
    this.titleInput.style.height = 'auto';
    this.titleInput.style.height = this.titleInput.scrollHeight+2+'px';
    this.titleHeight = this.titleInput.style.height;

    this.setState({title: evt.target.value});
  }

  bodyChange(evt){
    this.bodyInput.style.height = 'auto';
    this.bodyInput.style.height = this.bodyInput.scrollHeight+2+'px';
    this.bodyHeight = this.bodyInput.style.height;

    this.setState({body: evt.target.value});
  }

  render() {
    let enabledTab = ((this.state.title !== "") && (this.state.body !== ""));

    let mt = (this.windowHeight/2) - 96;
    let mb = (this.windowHeight/2) - 42;

    return (
      <div className="relative w-100" style={{height: '100%', top:124}}>
        <PC pathData={false} {...this.props}/>
        <div className="w-100 absolute" style={{top: mt}}>

          <div className="flex w-100 z-2 fixed" style={{top:132}}>
            <div className="w1 z-0" style={{flexGrow:1}}>
            </div>
            <div className="mw-688 w-100 z-0" style={{pointerEvents:"none"}}>
            </div>
            <SideTab enabled={enabledTab} postSubmit={this.postSubmit} />
          </div>

          <div className="flex absolute w-100"
            style={{top:0, marginBottom: mb}}
            ref={(el) => {this.inputArea = el}}>
            <div className="w1 z-0" style={{flexGrow:1}}>
            </div>
            <div className="flex-col w-100 mw-688 w-100 z-2">
              <textarea autoFocus
                className="header-2 w-100 b--none overflow-y-hidden"
                ref={(el) => {this.titleInput = el}}
                style={{resize:"none", marginBottom:8, height: this.titleHeight}}
                placeholder="Add a Title"
                onChange={this.titleChange.bind(this)}>
              </textarea>
              <textarea
                className="body-regular-400 w-100 z-2 b--none overflow-y-hidden"
                ref={(el) => {this.bodyInput = el}}
                style={{resize:"none", height: this.bodyHeight}}
                placeholder="And type away."
                onChange={this.bodyChange.bind(this)}>
              </textarea>
            </div>
            <div className="w1 z-0" style={{flexGrow:1, pointerEvents: "none"}}>
            </div>
          </div>

        </div>
      </div>
    );
  }
}
