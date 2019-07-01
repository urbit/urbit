import React, { Component } from 'react';
import classnames from 'classnames';
import { Link } from 'react-router-dom';
import { PathControl } from '/components/lib/path-control';
import { withRouter } from 'react-router';
import { store } from '/store';

const PC = withRouter(PathControl);

class FormLink extends Component {
  render(props){
    if (this.props.enabled) {
      return (
        <p className="body-large b z-2 pointer" onClick={this.props.action}>
          {this.props.body}
        </p>
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
      collaborators: [],
      page: 'main',
      awaiting: false,
    };
    this.titleChange = this.titleChange.bind(this);
    this.collaboratorChange = this.collaboratorChange.bind(this);
    this.firstPost = this.firstPost.bind(this);
    this.returnHome = this.returnHome.bind(this);
    this.addCollaborators = this.addCollaborators.bind(this);
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
        who: this.state.collaborators,
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
        who: this.state.collaborators,
      }
    }

    this.setState({
      awaiting: blogId
    });

    store.handleEvent({
      data: {
        spinner: true,
      }
    });

    this.props.api.action("write", "write-action", makeBlog);
    this.props.api.action("write", "write-action", sendInvites);
  }

  componentDidUpdate(prevProps, prevState) {
    if (this.state.awaiting) {
      if (this.props.pubs[this.state.awaiting]) {
        store.handleEvent({
          data: {
            spinner: false,
          }
        });
        this.props.history.push(`/~publish/~${window.ship}/${this.state.awaiting}`);
      }
    }
  }

  titleChange(evt){
    this.setState({title: evt.target.value});
  }

  collaboratorChange(evt){
    let collaborators = evt.target.value
      .trim()
      .split(",")
      .map(t => t.trim().substr(1));
    this.setState({collaborators: collaborators});
  }

  firstPost() {
    this.blogSubmit();
  }

  addCollaborators() {
    this.setState({page: 'addCollab'});
  }

  returnHome() {
    console.log("action return home");
  }

  render() {
    if (this.state.page === 'main') {
      return (
        <div>
          <div className="cf w-100 bg-white h-publish-header">
            <PC pathData={false} {...this.props}/>
          </div>
          <div className="h-inner dt center mw-688 w-100">
             <div className="flex-col dtc v-mid">
                <input autoFocus
                  className="header-2 b--none"
                  type="text" 
                  name="blogName"
                  placeholder="Add a Title"
                  onChange={this.titleChange}
                />

                <hr className="gray-30"/>

                <FormLink
                  enabled={(this.state.title !== '')}
                  action={this.firstPost}
                  body={"-> Create a first post"}
                />

                <hr className="gray-30"/>

                <FormLink
                  enabled={(this.state.title !== '')}
                  action={this.addCollaborators}
                  body={"-> Add Collaborators"}
                />

                <hr className="gray-30"/>

                <Link to="/~publish/recent" className="body-large b">
                  Cancel
                </Link>
             </div>
          </div>
        </div>
      );
    } else if (this.state.page === 'addCollab') {
      return (
        <div>
          <PC pathData={false} {...this.props}/>
          <div className="absolute w-100" style={{top: 124}}>
            <div className="h-inner dt center mw-688 w-100">
              <div className="flex-col dtc v-mid">
                 <input autoFocus
                   className="header-2 b--none"
                   type="text" 
                   name="blogName"
                   placeholder="Add a Title"
                   onChange={this.titleChange}
                 />

                 <p className="body-regular-400">
                   Who else can post to this blog?
                 </p>

                 <input className="body-regular-400 b--none w-100"
                   type="text"
                   name="collaborators"
                   placeholder="~ship-name, ~ship-name"
                   onChange={this.collaboratorChange}
                 />
                   

                 <hr className="gray-30"/>

                 <FormLink
                   enabled={(this.state.title !== '')}
                   action={this.firstPost}
                   body={"-> Save and create a first post"}
                 />

                 <hr className="gray-30"/>

                 <FormLink
                   enabled={(this.state.title !== '')}
                   action={this.returnHome}
                   body={"-> Save and return home"}
                 />

                 <hr className="gray-30"/>

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
