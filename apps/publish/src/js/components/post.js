import React, { Component } from 'react';
import classnames from 'classnames';
import { PostPreview } from '/components/lib/post-preview';
import moment from 'moment';
import { Link } from 'react-router-dom';
import { PostBody } from '/components/lib/post-body';
import { Comments } from '/components/lib/comments';
import { PathControl } from '/components/lib/path-control';
import { NextPrev } from '/components/lib/next-prev';
import { NotFound } from '/components/not-found';
import { withRouter } from 'react-router';
import _ from 'lodash';

const NF = withRouter(NotFound);

class Admin extends Component {
  constructor(props) {
    super(props);
  }

  render() {
    if (!this.props.enabled){
      return null;
    } else if (this.props.mode === 'view'){
      return (
        <div className="flex-col fr">
          <p className="label-regular gray-50 pointer tr b"
             onClick={this.props.editPost}>
            Edit
          </p>
          <p className="label-regular red pointer tr b"
             onClick={this.props.deletePost}>
            Delete
          </p>
        </div>
      );
    } else if (this.props.mode === 'edit'){
      return (
        <div className="flex-col fr">
          <p className="label-regular gray-50 pointer tr b"
             onClick={this.props.savePost}>
            Save
          </p>
          <p className="label-regular red pointer tr b"
             onClick={this.props.deletePost}>
            Delete
          </p>
        </div>
      );
    }
  }
}

export class Post extends Component {
  constructor(props){
    super(props);

    moment.updateLocale('en', {
      relativeTime: {
        past: function(input) {
          return input === 'just now'
            ? input
            : input + ' ago'
        },
        s : 'just now',
        future : 'in %s',
        m  : '1m',
        mm : '%dm',
        h  : '1h',
        hh : '%dh',
        d  : '1d',
        dd : '%dd',
        M  : '1 month',
        MM : '%d months',
        y  : '1 year',
        yy : '%d years',
      }
    });

    this.state = {
      mode: 'view',
      titleOriginal: '',
      bodyOriginal: '',
      title: '',
      body: '',
      awaitingEdit: false,
      awaitingLoad: false,
      awaitingDelete: false,
      ship: this.props.ship,
      blogId: this.props.blogId,
      postId: this.props.postId,
      blog: null,
      post: null,
      comments: null,
      pathData: [],
      temporary: false,
      notFound: false,
    }

    this.editPost = this.editPost.bind(this);
    this.deletePost = this.deletePost.bind(this);
    this.savePost = this.savePost.bind(this);
    this.titleChange = this.titleChange.bind(this);
    this.bodyChange = this.bodyChange.bind(this);

  }

  editPost() {
    this.setState({mode: 'edit'});
  }
  
  savePost() {
    if (this.state.title == this.state.titleOriginal &&
        this.state.body == this.state.bodyOriginal) {
      this.setState({mode: 'view'});
      return;
    }

    this.props.setSpinner(true);
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
    
    let data = {
      "edit-post": {
        who: this.state.ship,
        coll: this.props.blogId,
        name: this.props.postId,
        title: this.state.title,
        comments: this.state.post.info.comments,
        perm: permissions,
        content: this.state.body,

      },
    };

    this.setState({
      awaitingEdit: {
        ship: this.state.ship,
        blogId: this.props.blogId,
        postId: this.props.postId,
      }
    }, () => {
      this.props.api.action("publish", "publish-action", data)
    });
  }

  componentWillMount() {
    let ship = this.props.ship;
    let blogId = this.props.blogId;
    let postId = this.props.postId;

    if (ship !== window.ship) {
      
      let blog = _.get(this.props, `subs[${ship}][${blogId}]`, false);

      if (blog) {
        let post = _.get(blog, `posts[${postId}].post`, false);
        let comments = _.get(blog, `posts[${postId}].comments`, false);
        let blogUrl = `/~publish/${blog.info.owner}/${blog.info.filename}`;
        let postUrl = `${blogUrl}/${post.info.filename}`;

        this.setState({
          titleOriginal: post.info.title,
          bodyOriginal: post.raw,
          title: post.info.title,
          body: post.raw,
          blog: blog,
          post: post,
          comments: comments,
          pathData: [
            { text: "Home", url: "/~publish/recent" },
            { text: blog.info.title, url: blogUrl },
            { text: post.info.title, url: postUrl },
          ],
        });

        let read = {
          read: {
            who: ship,
            coll: blogId,
            post: postId,
          }
        };
        this.props.api.action("publish", "publish-action", read);

      } else {
        this.setState({
          awaitingLoad: {
            ship: ship,
            blogId: blogId,
            postId: postId,
          },
          temporary: true,
        });

        this.props.setSpinner(true);

        this.props.api.bind(`/collection/${blogId}`, "PUT", ship, "publish",
          this.handleEvent.bind(this),
          this.handleError.bind(this));
      }
    } else {
      let blog = _.get(this.props, `pubs[${blogId}]`, false);
      let post = _.get(blog, `posts[${postId}].post`, false);
      let comments = _.get(blog, `posts[${postId}].comments`, false);

      if (!blog || !post) {
        this.setState({notFound: true});
        return;
      } else {
        let blogUrl = `/~publish/${blog.info.owner}/${blog.info.filename}`;
        let postUrl = `${blogUrl}/${post.info.filename}`;

        this.setState({
          titleOriginal: post.info.title,
          bodyOriginal: post.raw,
          title: post.info.title,
          body: post.raw,
          blog: blog,
          post: post,
          comments: comments,
          pathData: [
            { text: "Home", url: "/~publish/recent" },
            { text: blog.info.title, url: blogUrl },
            { text: post.info.title, url: postUrl },
          ],
        });
      }
    }
  }

  handleEvent(diff) {
    if (diff.data.total) {
      let blog = diff.data.total.data;
      let post = blog.posts[this.state.postId].post;
      let comments = blog.posts[this.state.postId].comments;
      let blogUrl = `/~publish/${blog.info.owner}/${blog.info.filename}`;
      let postUrl = `${blogUrl}/${post.info.filename}`;

      this.setState({
        awaitingLoad: false,
        titleOriginal: post.info.title,
        bodyOriginal: post.raw,
        title: post.info.title,
        body: post.raw,
        blog: blog,
        post: post,
        comments: comments,
        pathData: [
          { text: "Home", url: "/~publish/recent" },
          { text: blog.info.title, url: blogUrl },
          { text: post.info.title, url: postUrl },
        ],
      });

      this.props.setSpinner(false);

    } else if (diff.data.collection) {
      let newBlog = this.state.blog;
      newBlog.info = diff.data.collection.data;
      this.setState({
        blog: newBlog,
      });
    } else if (diff.data.post) {
      this.setState({
        post: diff.data.post.data,
      });
    } else if (diff.data.comments) {
      this.setState({
        comments: diff.data.comments.data,
      });
    } else if (diff.data.remove) {
      // XX TODO Handle this properly
    }
  }

  handleError(err) {
    this.props.setSpinner(false);
    this.setState({notFound: true});
  }

  componentDidUpdate(prevProps, prevState) {
    if (this.state.notFound) return;

    let ship   = this.props.ship;
    let blogId = this.props.blogId;
    let postId = this.props.postId;

    let oldPost = prevState.post;
    let oldComments = prevState.comments;
    let oldBlog = prevState.blog;

    let post;
    let comments;
    let blog;

    if (ship === window.ship) {
      blog = _.get(this.props, `pubs[${blogId}]`, false);
      post = _.get(blog, `posts[${postId}].post`, false);
      comments = _.get(blog, `posts[${postId}].comments`, false);
    } else {
      blog = _.get(this.props, `subs[${ship}][${blogId}]`, false);
      post = _.get(blog, `posts[${postId}].post`, false);
      comments = _.get(blog, `posts[${postId}].comments`, false);
    }


    if (this.state.awaitingDelete && (post === false) && oldPost) {
      this.props.setSpinner(false);
      let redirect = `/~publish/~${this.props.ship}/${this.props.blogId}`;
      this.props.history.push(redirect);
      return;
    }

    if (!blog || !post) {
      this.setState({notFound: true});
      return;
    }

    if (this.state.awaitingEdit &&
       ((post.info.title != oldPost.info.title) ||
        (post.raw != oldPost.raw))) {

      this.setState({
        mode: 'view',
        titleOriginal: post.info.title,
        bodyOriginal: post.raw,
        title: post.info.title,
        body: post.raw,
        awaitingEdit: false,
        post: post,
      });

      this.props.setSpinner(false);
    }

    if (!this.state.temporary){
      if (oldPost != post) {
        this.setState({
          titleOriginal: post.info.title,
          bodyOriginal: post.raw,
          post: post,
        });
      }
      if (oldComments != comments) {
        this.setState({comments: comments});
      }
      if (oldBlog != blog) {
        this.setState({blog: blog});
      }
    }
  }

  deletePost(){
    let del = {
      "delete-post": {
        coll: this.props.blogId,
        post: this.props.postId,
      }
    };
    this.props.setSpinner(true);
    this.setState({
      awaitingDelete: {
        ship: this.props.ship,
        blogId: this.props.blogId,
        postId: this.props.postId,
      }
    }, () => {
      this.props.api.action("publish", "publish-action", del);
    });
  }

  titleChange(evt){
    this.setState({title: evt.target.value});
  }

  bodyChange(evt){
    this.setState({body: evt.target.value});
  }

  render() {
    let adminEnabled = (this.props.ship === window.ship);

    if (this.state.notFound) {
      return (
        <NF/>
      );
    } else if (this.state.awaitingLoad) {
      return null;
    } else if (this.state.awaitingEdit) {
      return null;
    } else if (this.state.mode == 'view') {
      let blogLink = `/~publish/~${this.state.ship}/${this.props.blogId}`;
      let blogLinkText = `<- Back to ${this.state.blog.info.title}`;

      let date = moment(this.state.post.info["date-created"]).fromNow();
      let authorDate = `${this.state.post.info.creator} • ${date}`;
      let create = (this.props.ship === window.ship);
      return (
        <div>
          <PathControl pathData={this.state.pathData} create={create}/>
          <div className="absolute w-100" style={{top:124}}>
            <div className="mw-688 center mt4 flex-col" style={{flexBasis: 688}}>
              <Link to={blogLink}>
                <p className="body-regular">
                  {blogLinkText}
                </p>
              </Link>

              <h2 style={{wordWrap: "break-word"}}>{this.state.titleOriginal}</h2>

              <div className="mb4">
                <p className="fl label-small gray-50">{authorDate}</p>
                <Admin
                  enabled={adminEnabled} 
                  mode="view"
                  editPost={this.editPost}
                  deletePost={this.deletePost}
                />
              </div>

              <div className="cb">
                <PostBody
                  body={this.state.post.body} 
                />
              </div>

              <hr className="gray-50 w-680 mt4"/>
              <NextPrev blog={this.state.blog} postId={this.props.postId} />
            
              <Comments comments={this.state.comments} 
                api={this.props.api}
                ship={this.props.ship}
                blogId={this.props.blogId}
                postId={this.props.postId}
                setSpinner={this.props.setSpinner}
              />
            </div>
          </div>
        </div>
      );

    } else if (this.state.mode == 'edit') {
      let blogLink = `/~publish/~${this.state.ship}/${this.props.blogId}`;
      let blogLinkText = `<- Back to ${this.state.blog.info.title}`;

      let date = moment(this.state.post.info["date-created"]).fromNow();
      let authorDate = `${this.state.post.info.creator} • ${date}`;
      let create = (this.props.ship === window.ship);
      return (
        <div>
          <PathControl pathData={this.state.pathData} create={create}/>
          <div className="absolute w-100" style={{top:124}}>
            <div className="mw-688 center mt4 flex-col" style={{flexBasis: 688}}>
              <Link to={blogLink}>
                <p className="body-regular">
                  {blogLinkText}
                </p>
              </Link>

              <input className="header-2 w-100"
                type="text"
                name="postName"
                defaultValue={this.state.titleOriginal}
                onChange={this.titleChange}
              />

              <div className="mb4">
                <p className="fl label-small gray-50">{authorDate}</p>
                <Admin
                  enabled={adminEnabled} 
                  mode="edit"
                  savePost={this.savePost}
                  deletePost={this.deletePost}
                />
              </div>

              <textarea className="cb body-regular-400 w-100 h5"
                style={{resize:"none"}}
                type="text"
                name="postBody"
                onChange={this.bodyChange}
                defaultValue={this.state.bodyOriginal}>
              </textarea>

              <hr className="gray-50 w-680 mt4"/>
              <NextPrev blog={this.state.blog} postId={this.props.postId} />
            
              <Comments comments={this.state.comments} 
                api={this.props.api}
                ship={this.props.ship}
                blogId={this.props.blogId}
                postId={this.props.postId}
                setSpinner={this.props.setSpinner}
              />
            </div>
          </div>
        </div>
      );
    }
  }
}

