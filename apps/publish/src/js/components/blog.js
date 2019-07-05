import React, { Component } from 'react';
import classnames from 'classnames';
import { PostPreview } from '/components/lib/post-preview';
import _ from 'lodash';
import { PathControl } from '/components/lib/path-control';
import { withRouter } from 'react-router';

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

      this.props.setSpinner(false);
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

      this.props.setSpinner(true);

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

    let contributors = " and X others";       // XX backend work
    let subscribers = "~bitpyx-dildus and X others"; // XX backend work

    if (this.state.awaiting) {
      return null;
    } else {
      return (
        <div>
          <PathControl pathData={data.pathData}/>
          <div className="absolute"
            style={{top:124, marginLeft: 16, marginRight: 16, marginTop: 32}}>
            <div className="flex-col">
              <h2>{data.blogTitle}</h2>
              <div className="flex" style={{marginTop: 22}}>
                <div style={{flexBasis: 160, marginRight:16}}>
                  <p className="gray-50 label-small-2 b">Host</p>
                  <p className="label-small-2">{data.blogHost}</p>
                </div>
                <div style={{flexBasis: 160, marginRight:16}}>
                  <p className="gray-50 label-small-2 b">Contributors</p>
                  <p className="label-small-2">{contributors}</p>
                </div>
                <div style={{flexBasis: 160, marginRight: 16}}>
                  <p className="gray-50 label-small-2 b">Subscribers</p>
                  <p className="label-small-2">{subscribers}</p>
                  <p className="label-small-2 b">Subscribe</p>
                </div>
              </div>
              <div className="flex flex-wrap" style={{marginTop: 48}}>
                {posts}
              </div>
            </div>
          </div>
        </div>
      );
    }
  }
}

