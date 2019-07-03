import React, { Component } from 'react';
import classnames from 'classnames';
import { PostPreview } from '/components/post-preview';
import _ from 'lodash';
import { PathControl } from '/components/lib/path-control';
import { withRouter } from 'react-router';
import { store } from '/store';

const PC = withRouter(PathControl);

export class Blog extends Component {
  constructor(props){
    super(props);

    this.state = {
      awaiting: false,
      postProps: [],
      blogTitle: '',
      blogHost: '',
      pathData: [],
      temporary: false,
    };
  }

  handleEvent(diff) {
    if (diff.data.total) {
      let blog = diff.data.total.data;
      this.setState({
        postProps: this.buildPosts(blog),
        blog: blog,
        blogTitle: blog.info.title,
        blogHost: blog.info.owner,
        awaiting: false,
        pathData: [
          { text: "Home", url: "/~publish/recent" },
          { text: blog.info.title, 
            url: `/~publish/${blog.info.owner}/${blog.info.filename}` }
        ],
      });

      store.handleEvent({
        data: {
          spinner: false,
        }
      });
    }
  }

  handleError(err) {
  }

  componentWillMount() {
    let ship = this.props.ship;
    let blogId = this.props.blogId;
    let blog = (ship == window.ship)
      ?  _.get(this.props, `pubs[${blogId}]`, false)
      :  _.get(this.props, `subs[${ship}][${blogId}]`, false);

    let temporary = (!(blog) && (ship != window.ship));

    if (temporary) {
      this.setState({
        awaiting: {
          ship: ship,
          blogId: blogId,
        },
        temporary: true,
      });

      store.handleEvent({
        data: {
          spinner: true,
        }
      });

      this.props.api.bind(`/collection/${blogId}`, "PUT", ship, "write",
        this.handleEvent.bind(this),
        this.handleError.bind(this));
    }
  }

  buildPosts(blog){
    let pinProps = blog.order.pin.map((postId) => {
      let post = blog.posts[postId];
      return this.buildPostPreviewProps(post, blog, true);
    });

    let unpinProps = blog.order.unpin.map((postId) => {
      let post = blog.posts[postId];
      return this.buildPostPreviewProps(post, blog, false);
    });

    return pinProps.concat(unpinProps);
  }

  buildPostPreviewProps(post, blog, pinned){
    return {
      postTitle: post.post.info.title,
      postName:  post.post.info.filename,
      postBody: post.post.body,
      numComments: post.comments.length,
      collectionTitle: blog.info.title,
      collectionName:  blog.info.filename,
      author: post.post.info.creator,
      blogOwner: blog.info.owner,
      date: post.post.info["date-created"],
      pinned: pinned,
    }
  }

  buildData(){
    let blog = (this.props.ship == window.ship)
      ?  _.get(this.props, `pubs[${this.props.blogId}]`, false)
      :  _.get(this.props, `subs[${this.props.ship}][${this.props.blogId}]`, false);

    if (this.state.temporary) {
      return {
        postProps: this.state.postProps,
        blogTitle: this.state.blogTitle,
        blogHost: this.state.blogHost,
        pathData: this.state.pathData,
      };
    } else {
      return {
        postProps: this.buildPosts(blog),
        blogTitle: blog.info.title,
        blogHost: blog.info.owner,
        pathData: [
          { text: "Home", url: "/~publish/recent" },
          { text: blog.info.title, 
            url: `/~publish/${blog.info.owner}/${blog.info.filename}` }
        ],
      };
    }
  }

  render() {
    let data = this.buildData();

    let posts = data.postProps.map((post, key) => {
      return (
        <PostPreview
          post={post}
          key={key}
        />
      );
    });

    let contributers = " and X others";       // XX backend work
    let subscribers = "~bitpyx-dildus and X others"; // XX backend work

    if (this.state.awaiting) {
      return null;
    } else {
      return (
        <div>
          <PathControl pathData={data.pathData}/>
          <div className="absolute w-100" style={{top:124}}>
            <div className="flex-col">
              <h2>{data.blogTitle}</h2>
              <div className="flex">
                <div style={{flexBasis: 350}}>
                  <p>Host</p>
                  <p>{data.blogHost}</p>
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
          </div>
        </div>
      );
    }
  }
}

