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

  render() {
    let ship = this.props.ship;
    let blog = this.retrieveColl(this.props.blogId, this.props.ship);
    let post = this.retrievePost(this.props.postId, this.props.blogId, this.props.ship);
    let comments = this.retrieveComments(this.props.postId, this.props.blogId, this.props.ship);

    let blogLink = `/~publish/~${this.props.ship}/${this.props.blogId}`;
    let blogLinkText = `<- Back to ${blog.info.title}`;

    let editLink = `/~publish/~${this.props.ship}/${this.props.blogId}/${this.props.postId}/edit`;

    let date = moment(post.info["date-created"]).fromNow();
    let authorDate = `${post.info.creator} • ${date}`;

    // change unpin to concatenation of pinned and unpinned
    let morePosts = blog.order.unpin.slice(0,10).map((pid, i) => {

      let p = this.retrievePost(pid, this.props.blogId, this.props.ship);
      let color = (pid == this.props.postId) ? "black" : "gray-50";
      let postLink = `/~publish/~${this.props.ship}/${this.props.blogId}/${pid}`;
      return (
        <Link to={postLink} className="label-regular" key={i}>
          <p className={color}>{p.info.title}</p>
        </Link>
      );
    });

    return (
      <div className="mw-688 center mt4 flex-col" style={{flexBasis: 688}}>
        <Link to={blogLink}>
          <p className="body-regular">
            {blogLinkText}
          </p>
        </Link>

        <h2>{post.info.title}</h2>

        <div className="mb4">
          <p className="fl label-small gray-50">{authorDate}</p>
          <Link to={editLink}>
            <p className="label-regular gray-50 fr">Edit</p>
          </Link>
        </div>

        <div className="cb">
          <PostBody
            body={post.body} 
          />
        </div>

        <hr className="gray-50 w-680"/>

        <div className="cb mt3 mb4">
          <p className="gray-50 body-large b">
            {comments.length}
            <span className="black">
              Comments
            </span>
          </p>
          <p className="cl body-regular">
            ↓ Show Comments
          </p>
        </div>

        <hr className="gray-50 w-680"/>

        <div className="cb flex-col">
          <p className="label-regular b mb1">{blog.info.title}</p>
          <p className="label-regular gray-30 mb2">Hosted by {blog.info.owner}</p>
          {morePosts}
        </div>
      </div>
    );
  }
}

