  type P = HTMLElementTagNameMap["p"];
  //   ^ The TS sdlib features a map from tag names to their corresponding element types

  function querySelector<S extends string>(
    selector: S
  ): SelectorToElement<S> | null {
    return document.querySelector(selector) as any;
  }

  type Split<Str, Separator extends string> =
    Str extends `${infer First}${Separator}${infer Rest}`
      ? [First, ...Split<Rest, Separator>]
      : [Str]
  
  type GetLastElem<T> =
    T extends [...any, infer Last]
      ? Last
      : never
  
  type NonSpaceSelectors = "." | "[" | ":"

  type AnyHTMLTag = keyof HTMLElementTagNameMap

  type As<T, U> = T extends U ? T : never

  type SelectorToElement<Selector> =
    GetLastElem<Split<Selector, " ">> extends infer Result // Caching/Storing the output of GetLastElem<Split<Selector, " ">> in Result
      ? Result extends AnyHTMLTag
        ? HTMLElementTagNameMap[Result]
        : Result extends `${infer Tag}${NonSpaceSelectors}${string}`
          ? HTMLElementTagNameMap[As<Tag, AnyHTMLTag>]
          : Element
      : never

const example = querySelector("div > p.first a[href*='sample']")
