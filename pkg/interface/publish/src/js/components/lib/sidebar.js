import React, { Component } from 'react'
import { Route, Link } from 'react-router-dom';
import { NotebookItem } from './notebook-item';

export class Sidebar extends Component {
  constructor(props) {
    super(props);
    this.state = {
      sort: "oldest",
      sortedBooks: new Map()
    }
    this.sort = this.sort.bind(this);
    this.sortChange = this.sortChange.bind(this);
  }

  componentDidMount() {
    this.sort();
  }

  componentDidUpdate(prevProps, prevState) {
    if ((prevState.sort !== this.state.sort) || (prevProps !== this.props)) {
      this.sort();
    }
  }

  sort() {
    let { props, state } = this;
    let notebooks = new Map();
    Object.keys(props.notebooks).map(host => {
      Object.keys(props.notebooks[host]).map(notebook => {
        let title = `${host}/${notebook}`;
        notebooks.set(title, props.notebooks[host][notebook])
      })
    });
    switch (state.sort) {
      case "oldest":
        notebooks = new Map(
          [...notebooks.entries()].sort(
            (a, b) => a[1]["date-created"] - b[1]["date-created"]
          )
        );
        break;
      case "newest":
        notebooks = new Map(
          [...notebooks.entries()].sort(
            (a, b) => b[1]["date-created"] - a[1]["date-created"]
          )
        );
        break;
      case "alphabetical":
        notebooks = new Map(
          [...notebooks.entries()].sort((a, b) => {
            if (a[1]["title"].toLowerCase() < b[1]["title"].toLowerCase()) {
              return -1;
            }
            if (a[1]["title"].toLowerCase() > b[1]["title"].toLowerCase()) {
              return 1;
            }
            return 0;
          })
        );
        break;
      case "reverseAlphabetical":
        notebooks = new Map(
          [...notebooks.entries()].sort((a, b) => {
            if (a[1]["title"].toLowerCase() > b[1]["title"].toLowerCase()) {
              return -1;
            }
            if (a[1]["title"].toLowerCase() < b[1]["title"].toLowerCase()) {
              return 1;
            }
            return 0;
          })
        );
        break;
      default:
        break;
    }
    this.setState({ sortedBooks: notebooks });
  }

  sortChange(event) {
    this.setState({sort: event.target.value});
  }

  render() {
    const { props, state } = this;
    let activeClasses = (props.active === "sidebar") ? " " : "dn-s ";
    let hiddenClasses = true;
    if (props.popout) {
      hiddenClasses = false;
    } else {
      hiddenClasses = props.sidebarShown;
    };

    let notebookItems = [...state.sortedBooks].map(([path, book]) => {
      let selected = (props.path === path);
      let author = path.split("/")[0];
      return (
        <NotebookItem
          key={book.title}
          title={book.title}
          author={author}
          path={path}
          total={book["num-notes"]}
          unreadCount={book["num-unread"]}
          selected={selected}
        />
      );
    })

    let notebooks = <div>{notebookItems}</div>

    return (
      <div
        className={
          "bn br-m br-l br-xl b--gray4 b--gray2-d lh-copy h-100 " +
          "flex-shrink-0 mw-300-ns pt3 pt0-m pt0-l pt0-xl relative " +
          activeClasses +
          (hiddenClasses ? "flex-basis-100-s flex-basis-30-ns" : "dn")
        }>
        <a className="db dn-m dn-l dn-xl f8 pb3 pl3" href="/">
          ⟵ Landscape
        </a>
        <div className="w-100">
          <Link to="/~publish/new" className="green2 mr4 f9 pl4 pt4 dib">
            New Notebook
          </Link>
          <Link to="/~publish/join" className="f9 gray2">
            Join Notebook
          </Link>
          <h2
            className={`f8 pt3 pr4 pb3 pl3 black c-default bb b--gray4 mb2
             dn-m dn-l dn-xl`}>
            Your Notebooks
          </h2>
          <div className="dropdown relative bb b--gray4">
            <select
              style={{ WebkitAppearance: "none" }}
              className="pl4 pv6 f9 bg-white bg-black-d white-d bn w-100 inter"
              value={this.state.sort}
              onChange={this.sortChange}>
              <option value="oldest">Oldest Notebooks First</option>
              <option value="newest">Newest Notebooks First</option>
              <option value="alphabetical">Alphabetical A -> Z</option>
              <option value="reverseAlphabetical">Alphabetical Z -> A</option>
            </select>
          </div>
        </div>
        <div className="overflow-y-scroll h-100">{notebooks}</div>
      </div>
    );
  }
}

export default Sidebar;