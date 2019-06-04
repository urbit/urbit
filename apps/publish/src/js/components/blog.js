import React, { Component } from 'react';
import classnames from 'classnames';
import { PostPreview } from '/components/post-preview';


export class Blog extends Component {
  constructor(props){
    super(props)
  }


  buildPosts(){
    let blogId = this.props.blogId;
    let ship = this.props.ship;
    let blog = this.retrieveColl(blogId, ship);

    console.log("buildposts", blog);

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
      postBody: pos.body,
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
    let blog = this.retrieveColl(this.props.blogId, this.props.ship);
    let postProps = this.buildPosts();
    let posts = postProps.map((post) => {
      return (
        <PostPreview
          post={post}
        />
      );
    });

    let host = blog.info.owner;
    let contributers = host + " and X others";       // XX backend work
    let subscribers = "~bitpyx-dildus and X others"; // XX backend work

    return (
      <div className="flex-col">
        <h2>{blog.info.title}</h2>
        <div className="flex">
          <div style={{flexBasis: 350}}>
            <p>Host</p>
            <p>{host}</p>
          </div>
          <div style={{flexBasis: 350}}>
            <p>Contributors</p>
            <p>{contributers}</p>
          </div>
          <div style={{flexBasis: 350}}>
            <p>Subscribers</p>
            <p>{subscribers}</p>
          </div>
        </div>
        <div className="flex flex-wrap">
          {posts}
        </div>
      </div>
    );
  }
}

