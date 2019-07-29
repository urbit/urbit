import React, { Component } from 'react';
import classnames from 'classnames';
import { Link } from 'react-router-dom';
import { PathControl } from '/components/lib/path-control';
import { withRouter } from 'react-router';
import urbitOb from 'urbit-ob';

const PC = withRouter(PathControl);

class FormLink extends Component {
  render(props){
    if (this.props.enabled) {
      return (
        <button className="body-large b z-2 pointer" onClick={this.props.action}>
          {this.props.body}
        </button>
      );
    }
    return (
      <p className="gray-30 b body-large">{this.props.body}</p>
    );
  }
}

export class NewBlog extends Component {
  constructor(props){
    super(props);

    this.state = {
      title: '',
      invites: [],
      page: 'main',
      awaiting: false,
      validInvites: true,
    };
    this.titleChange = this.titleChange.bind(this);
    this.invitesChange = this.invitesChange.bind(this);
    this.firstPost = this.firstPost.bind(this);
    this.returnHome = this.returnHome.bind(this);
    this.addInvites = this.addInvites.bind(this);
    this.blogSubmit = this.blogSubmit.bind(this);
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

  blogSubmit() {
    let ship = window.ship;
    let blogTitle = this.state.title;
    let blogId = this.stringToSymbol(blogTitle);

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

    let makeBlog = {
      "new-collection" : {
        name: blogId,
        title: blogTitle,
        comments: "open",
        "allow-edit": "all",
        perm: permissions,
      },
    };

    let sendInvites = {
      invite: {
        coll: blogId,
        title: blogTitle,
        who: this.state.invites,
      }
    }

    this.setState({
      awaiting: blogId
    });
  
    this.props.setSpinner(true);

    this.props.api.action("publish", "publish-action", makeBlog);
    this.props.api.action("publish", "publish-action", sendInvites);
  }

  componentDidUpdate(prevProps, prevState) {
    if (this.state.awaiting) {
      if (this.props.pubs[this.state.awaiting]) {
        this.props.setSpinner(false);
        
        if (this.state.redirect === 'new-post') {
          this.props.history.push("/~publish/new-post",
            {
              lastParams: {
                ship: `~${window.ship}`,
                blog: this.state.awaiting,
              }
            }
          );
        } else if (this.state.redirect === 'home') {
          this.props.history.push(
            `/~publish/~${window.ship}/${this.state.awaiting}`);
        }
      }
    }
  }

  titleChange(evt){
    this.setState({title: evt.target.value});
  }

  invitesChange(evt){
    let tokens = evt.target.value
      .trim()
      .split(/[\s,]+/)
      .map(t => t.trim());

    let valid = tokens.reduce((valid, s) => 
      valid && (((s !== '~') && urbitOb.isValidPatp(s) && s.includes('~')) ||
        (s === '')), true);

    if (valid) {
      this.setState({
        validInvites: true,
        invites: tokens.map(t => t.slice(1)),
      });
    } else {
      this.setState({validInvites: false});
    }
  }

  firstPost() {
    this.setState({redirect: "new-post"});
    this.blogSubmit();
  }

  addInvites() {
    this.setState({page: 'addInvites'});
  }

  returnHome() {
    this.setState({redirect: "home"});
    this.blogSubmit();
  }

  render() {
    if (this.state.page === 'main') {
      return (
        <div>
          <PC pathData={false} {...this.props}/>
          <div className="absolute w-100"
               style={{height: 'calc(100% - 124px)', top: 124}}>
            <div className="h-inner dt center mw-688 w-100">
              <div className="flex-col dtc v-mid">
                <input autoFocus
                  className="header-2 b--none"
                  type="text" 
                  name="blogName"
                  placeholder="Add a Title"
                  onChange={this.titleChange}
                />

                <hr className="gray-30" style={{marginTop:32, marginBottom: 32}}/>

                <FormLink
                  enabled={(this.state.title !== '')}
                  action={this.addInvites}
                  body={"-> Send Invites"}
                />

                <hr className="gray-30" style={{marginTop:32, marginBottom: 32}}/>

                <FormLink
                  enabled={(this.state.title !== '')}
                  action={this.firstPost}
                  body={"-> Create a first note"}
                />

                <hr className="gray-30" style={{marginTop:32, marginBottom: 32}}/>

                <Link to="/~publish/recent" className="body-large b">
                  Cancel
                </Link>
              </div>
            </div>
          </div>
        </div>
      );
    } else if (this.state.page === 'addInvites') {
      let enableButtons = ((this.state.title !== '') && this.state.validInvites);
      let invitesStyle = (this.state.validInvites)
        ?  "body-regular-400 b--none w-100"
        :  "body-regular-400 b--none w-100 red";

      return (
        <div>
          <PC pathData={false} {...this.props}/>
          <div className="absolute w-100"
               style={{height: 'calc(100% - 124px)', top: 124}}>
            <div className="h-inner dt center mw-688 w-100">
              <div className="flex-col dtc v-mid">
                <input autoFocus
                  className="header-2 b--none"
                  type="text" 
                  name="blogName"
                  placeholder="Add a Title"
                  onChange={this.titleChange}
                />

                <p className="body-regular-400" style={{marginTop:25, marginBottom:27}}>
                  Who is invited to read this notebook?
                </p>

                <input className={invitesStyle}
                  style={{caretColor: "black"}}
                  type="text"
                  name="invites"
                  placeholder="~ship-name, ~ship-name"
                  onChange={this.invitesChange}
                />

                <hr className="gray-30" style={{marginTop:32, marginBottom: 32}}/>

                <FormLink
                  enabled={enableButtons}
                  action={this.firstPost}
                  body={"-> Save and create a first note"}
                />

                <hr className="gray-30" style={{marginTop:32, marginBottom: 32}}/>

                <FormLink
                  enabled={enableButtons}
                  action={this.returnHome}
                  body={"-> Save and return home"}
                />

                <hr className="gray-30" style={{marginTop:32, marginBottom: 32}}/>

                <Link to="/~publish/recent" className="body-large b">
                  Cancel
                </Link>
              </div>
            </div>
          </div>
        </div>
      );

    }
  }
}
