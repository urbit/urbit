import React, { Component } from 'react';
import { Post } from './post';


const PAGE_SIZE = 50;

export class PostList extends Component {
  constructor(props) {
    super(props);
    this.scrollRef = React.createRef();

    this.state = {
      page: 0
    };
  }

  previousPage() {
    const { state } = this;
    this.setState({
      page: state.page - 1
    }, () => {
      this.scrollRef.current.scrollTo(0, this.scrollRef.current.scrollHeight);
    });
  }

  nextPage() {
    const { state } = this;
    this.setState({
      page: state.page + 1
    }, () => {
      this.scrollRef.current.scrollTo(0, 0);
    });
  }

  paginatedNodes() {
    const { props, state } = this;
    let nodes = Array.from(props.graph).sort((a,b) => {
      return b[0] - a[0];
    });

    return {
      nodes: nodes.slice(
        PAGE_SIZE * state.page,
        PAGE_SIZE * (state.page + 1)
      ),
      length: nodes.length
    };
  }

  render() {
    const { props, state } = this;
    let { nodes, length } = this.paginatedNodes();

    return (
      <div className={
             'overflow-y-scroll bg-white bg-gray0-d pt3 pb2' +
             'flex flex-column relative'
           }
           style={{ height: '100%', resize: 'vertical' }}
           ref={this.scrollRef}>
        { (state.page !== 0) ?
          (
            <button onClick={this.previousPage.bind(this)}>
              Load New
            </button>
          ) : null
        }
        { nodes.map((node, i) => {
            let post = node[1].post;

            return (
              <Post
                key={post.index}
                msg={post}
                resource={props.resource}
                index={post.index}
                history={props.history}
              />
            );
          })
        }
        { (PAGE_SIZE * (state.page + 1) < length) ?
          (
            <button onClick={this.nextPage.bind(this)}>
              Load Old
            </button>
          ) : null
        }
      </div>
    );
  }

}
