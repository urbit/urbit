import React, { Component } from 'react'
import { Route, Link } from 'react-router-dom';
import { Dropdown } from './dropdown';
import { NotebookItem } from './notebook-item';
import { SidebarInvite } from './sidebar-invite';

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
            (a, b) => {
              if ((a[1]) && (b[1])) {
              return a[1]["date-created"] - b[1]["date-created"]
              }
            }
          )
        );
        break;
      case "newest":
        notebooks = new Map(
          [...notebooks.entries()].sort(
            (a, b) => {
              if ((a[1]) && (b[1])) {
              return b[1]["date-created"] - a[1]["date-created"]
              }
            }
          )
        );
        break;
      case "alphabetical":
        notebooks = new Map(
          [...notebooks.entries()].sort((a, b) => {
            if ((a[1]) && (b[1])) {
              if (a[1]["title"].toLowerCase() < b[1]["title"].toLowerCase()) {
                return -1;
              }
              if (a[1]["title"].toLowerCase() > b[1]["title"].toLowerCase()) {
                return 1;
              }
              return 0;
            }
          })
        );
        break;
      case "reverseAlphabetical":
        notebooks = new Map(
          [...notebooks.entries()].sort((a, b) => {
            if ((a[1]) && (b[1])) {
              if (a[1]["title"].toLowerCase() > b[1]["title"].toLowerCase()) {
                return -1;
              }
              if (a[1]["title"].toLowerCase() < b[1]["title"].toLowerCase()) {
                return 1;
              }
              return 0;
            }
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

    let sidebarInvites =  !(props.invites && props.invites['/publish'])
      ? null
      : Object.keys(props.invites['/publish'])
          .map((uid, i) => {
            return (
              <SidebarInvite
                uid={uid}
                invite={props.invites['/publish'][uid]}
                key={i} />
            )
        });

    let notebookItems = [...state.sortedBooks].map(([path, book]) => {
      let selected = (props.path === path);
      let author = path.split("/")[0];
      return (
        <NotebookItem
          key={book.title}
          title={book.title}
          author={author}
          contacts={props.contacts}
          contactsPath={book["subscribers-group-path"]}
          path={path}
          total={book["num-notes"]}
          unreadCount={book["num-unread"]}
          selected={selected}
        />
      );
    })

    return (
      <div
        className={
          "bn br-m br-l br-xl b--gray4 b--gray2-d lh-copy h-100 " +
          "flex-shrink-0 pt3 pt0-m pt0-l pt0-xl relative " +
          "overflow-y-hidden " + activeClasses +
          (hiddenClasses ? "flex-basis-100-s flex-basis-300-ns" : "dn")
        }>
        <a className="db dn-m dn-l dn-xl f9 pb3 pl3" href="/">
          ⟵ Landscape
        </a>
        <div className="w-100 f9">
          <Link to="/~publish/new" className="green2 mr4 f9 pl4 pt4 dib">
            New Notebook
          </Link>
          <Link to="/~publish/join" className="f9 gray2">
            Join Notebook
          </Link>
          <div className="pl2 pv2 bb b--gray4">
          <Dropdown
            width="16rem"
            align="left"
            options={[
              {
                cls: "w-100 tl pointer db ph2 pv3 hover-bg-gray4",
                txt: "Oldest",
                action: () => {this.setState({sort: "oldest"})}
              },
              {
                cls: "w-100 tl pointer db ph2 pv3 hover-bg-gray4",
                txt: "Newest",
                action: () => {this.setState({sort: "newest"})}
              },
              {
                cls: "w-100 tl pointer db ph2 pv3 hover-bg-gray4",
                txt: "A -> Z",
                action: () => {this.setState({sort: "alphabetical"})}
              },
              {
                cls: "w-100 tl pointer db ph2 pv3 hover-bg-gray4",
                txt: "Z -> A",
                action: () => {this.setState({sort: "reverseAlphabetical"})}
              }
            ]}
            buttonText="Sort By"
          />
          </div>
        </div>
        <div className="overflow-y-auto pb1"
        style={{height: "calc(100% - 82px)"}}>
          {sidebarInvites}
          {notebookItems}
        </div>
      </div>
    );
  }
}

export default Sidebar;
