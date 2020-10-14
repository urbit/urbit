import { PatpNoSig, Path } from "./noun";

export type LinkCollections = {
  [p in Path]: Collection;
};

export type LinkSeen = {
  [p in Path]: {
    [url: string]: boolean;
  };
};

export type Pagination<S> = {
  local: LocalPages;
  [p: number]: S[];
  totalItems: number;
  totalPages: number;
}

export type LinkComments = {
  [p in  Path]: {
    [url: string]: Pagination<LinkComment> & {
      totalItems: number;
      totalPages: number;
    }
  }
}

interface LinkComment {
  ship: PatpNoSig;
  time: number;
  udon: string;
}

interface CollectionStats {
  unseenCount: number; 
}

type LocalPages = {
  [p: number]: boolean;
}

type Collection = CollectionStats & Pagination<Link>;

interface Link {
  commentCount: number;
  seen: boolean;
  ship: PatpNoSig;
  time: number;
  title: string;
  url: string;
}

interface LinkInitialSubmissions {
  'initial-submissions': {
     [p in Path]: CollectionStats & {
       pageNumber?: number;
       pages?: Link[];
     }
  };
};

interface LinkUpdateSubmission {
  'submissions': {
    path: Path;
    pages: Link[];
  }
}

interface LinkInitialDiscussion {
  'intitial-discussion': {
    path: Path;
    url: string;
    page: Comment[];
    totalItems: number;
    totalPages: number;
    pageNumber: number;
  }
}

export type LinkUpdate =
  LinkInitialSubmissions
| LinkUpdateSubmission
| LinkInitialDiscussion;
