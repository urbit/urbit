import React, { Component } from 'react';
import classnames from 'classnames';
import { Link } from 'react-router-dom';

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

    console.log("new post", this.props);

    this.state = {
      title: "",
      body: "",
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
        coll: blogId,
        name: postId,
        title: postTitle,
        comments: "open",
        perm: permissions,
        content: content,
      },
    };

    console.log("postSubmit", data);
    this.props.api.action("write", "write-action", data).then((foo) => {
      console.log("foo", foo);
    });
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
