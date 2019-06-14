import React, { Component } from 'react';
import classnames from 'classnames';
import { PostPreview } from '/components/post-preview';
import moment from 'moment';
import { Link } from 'react-router-dom';
import { PostBody } from '/components/post-body';

export class Post extends Component {
  constructor(props){
    super(props)

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
    let blog = this.retrieveColl(this.props.blogId, this.props.ship);
    let post = this.retrievePost(this.props.postId, this.props.blogId, this.props.ship);
    let comments = this.retrieveComments(this.props.postId, this.props.blogId, this.props.ship);

    this.state = {
      mode: 'view',
      titleOriginal: post.info.title,
      bodyOriginal: post.raw,
      title: post.info.title,
      body: post.raw,
      awaiting: false,
      ship: this.props.ship,
      blog: blog,
      post: post,
      comments: comments,
    }

    this.editPost = this.editPost.bind(this);
    this.savePost = this.savePost.bind(this);
    this.titleChange = this.titleChange.bind(this);
    this.bodyChange = this.bodyChange.bind(this);

  }


  buildPosts(){
    let blogId = this.props.blogId;
    let ship = this.props.ship;
    let blog = this.retrieveColl(blogId, ship);

    let pinProps = blog.order.pin.map((post) => {
      return this.buildPostPreviewProps(post, blogId, ship, true);
    });

    let unpinProps = blog.order.unpin.map((post) => {
      return this.buildPostPreviewProps(post, blogId, ship, false);
    });

    return pinProps.concat(unpinProps);
  }

  buildPostPreviewProps(post, coll, who, pinned){
    let pos = this.retrievePost(post, coll, who);
    let col = this.retrieveColl(coll, who);
    let com = this.retrieveComments(post, coll, who);

    return {
      postTitle: pos.info.title,
      postName:  post,
      postSnippet: "body snippet",
      numComments: com.length,
      collectionTitle: col.title,
      collectionName:  coll,
      author: who,
      date: pos.info["date-created"],
      pinned: pinned,
    }
  }
  

  retrievePost(post, coll, who) {
    if (who === window.ship) {
      return this.props.pubs[coll].posts[post].post;
    } else {
      return this.props.subs[who][coll].posts[post].post;
    }
  }

  retrieveComments(post, coll, who) {
    if (who === window.ship) {
      return this.props.pubs[coll].posts[post].comments;
    } else {
      return this.props.subs[who][coll].posts[post].comments;
    }
  }

  retrieveColl(coll, who) {
    if (who === window.ship) {
      return this.props.pubs[coll];
    } else {
      return this.props.subs[who][coll];
    }
  }

  editPost() {
    this.setState({mode: 'edit'});
  }
  
  savePost() {
    if (this.state.title == this.state.titleOriginal &&
        this.state.body == this.state.bodyOriginal) {
      return;
    }

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
      awaiting: {
        ship: this.state.ship,
        blogId: this.props.blogId,
        postId: this.props.postId,
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
        let oldPost = prevState.post;

        let post = _.get(this.props,
          `pubs[${blogId}].posts[${postId}].post`, false);

        if ((post.info.title != oldPost.info.title) ||
            (post.raw != oldPost.raw)) {

          this.setState({
            mode: 'view',
            titleOriginal: post.info.title,
            bodyOriginal: post.raw,
            title: post.info.title,
            body: post.raw,
            awaiting: false,
            post: post,
          });
        }
      } else {

        let oldPost = prevState.post;

        let post = _.get(this.props,
          `subs[${ship}][${blogId}].posts[${postId}].post`, false);
        
        if ((post.info.title != oldPost.info.title) ||
            (post.raw != oldPost.raw)) {
          this.setState({
            mode: 'view',
            titleOriginal: post.info.title,
            bodyOriginal: post.raw,
            title: post.info.title,
            body: post.raw,
            awaiting: false,
            post: post,
          });
        }
      }
    }
  }


  titleChange(evt){
    this.setState({title: evt.target.value});
  }

  bodyChange(evt){
    this.setState({body: evt.target.value});
  }

  render() {
    let blogLink = `/~publish/~${this.state.ship}/${this.props.blogId}`;
    let blogLinkText = `<- Back to ${this.state.blog.info.title}`;

    let date = moment(this.state.post.info["date-created"]).fromNow();
    let authorDate = `${this.state.post.info.creator} • ${date}`;



    if (this.state.mode == 'view') {
      return (
        <div className="mw-688 center mt4 flex-col" style={{flexBasis: 688}}>
          <Link to={blogLink}>
            <p className="body-regular">
              {blogLinkText}
            </p>
          </Link>

          <h2>{this.state.titleOriginal}</h2>

          <div className="mb4">
            <p className="fl label-small gray-50">{authorDate}</p>
            <p className="label-regular gray-50 fr pointer"
               onClick={this.editPost}>
              Edit
            </p>
          </div>

          <div className="cb">
            <PostBody
              body={this.state.post.body} 
            />
          </div>

          <hr className="gray-50 w-680"/>
        
          <hr className="gray-50 w-680"/>

          <div className="cb mt3 mb4">
            <p className="gray-50 body-large b">
              {this.state.comments.length}
              <span className="black">
                Comments
              </span>
            </p>
            <p className="cl body-regular">
              + Show Comments
            </p>
          </div>
        </div>
      );

    } else if (this.state.mode == 'edit') {
      return (
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
            <p className="label-regular gray-50 fr pointer"
               onClick={this.savePost}>
              Save
            </p>
          </div>

          <textarea className="cb body-regular-400 w-100 h5"
            style={{resize:"none"}}
            type="text"
            name="postBody"
            onChange={this.bodyChange}
            defaultValue={this.state.bodyOriginal}>
          </textarea>

          <hr className="gray-50 w-680"/>
        
          <hr className="gray-50 w-680"/>

          <div className="cb mt3 mb4">
            <p className="gray-50 body-large b">
              {this.state.comments.length}
              <span className="black">
                Comments
              </span>
            </p>
            <p className="cl body-regular">
              + Show Comments
            </p>
          </div>
        </div>
      );
    }
  }
}

