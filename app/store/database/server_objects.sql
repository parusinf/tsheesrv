create or replace function F_PSTS_CONFIRMED
(
  nCOMPANY                  in number,       -- Организация
  nRN                       in number,       -- RN группы (табеля)
  dWORKDATE                 in date          -- Период табеля
)
return number                                -- 0: Табель не утвержден, 1: Табель утвержден
as
  nALL_CONF                    number;
begin
  select case when CMP_NUM(count(C.PSORGGRP),sum(PR.TAB_CONF))=0 or count(C.PSORGGRP)=0 then 0 else 1 end
    into nALL_CONF
    from PSPAYCARD    C,
         PSPAYCARDPRD PR
   where C.RN         = PR.PRN (+)
     and(PR.YEAR  (+) = extract(year  from dWORKDATE)
     and PR.MONTH (+) = extract(month from dWORKDATE))
     and C.DATE_FROM <= last_day(dWORKDATE)
     and(C.DATE_TO   >= trunc(dWORKDATE) or C.DATE_TO is null)
     and C.PSORGGRP   = nRN
     and C.COMPANY    = nCOMPANY;
  return nALL_CONF;
end;
/
show errors;

create or replace function F_PSTS_SAL_SHEET
(
  nCOMPANY        in number,            -- Организация
  nRN             in number,            -- RN группы (табеля)
  dWORKDATE       in date               -- Период табеля
)
return number                                -- 0: Ведомость не сформирована, 1: Ведомость сформирована
as
  nSAL_SHEET      number;
begin
  select case when exists
           ( select /*+ INDEX(L I_DOCLINKS_IN_DOCUMENT) */ null
               from DOCLINKS L,
                    PSPAYCARD    C,
                    PSSHEET       S
              where L.IN_DOCUMENT  = C.RN
                and L.IN_UNITCODE  = 'ParentPayCards'
                and L.OUT_UNITCODE = 'ParentSalarySheets'
                and C.PSORGGRP = nRN
                and C.COMPANY = nCOMPANY
                and L.OUT_DOCUMENT = S.RN
                and extract(year  from dWORKDATE) = S.YEAR
                and extract(month from dWORKDATE) = S.MONTH
                and C.DATE_FROM <= last_day(dWORKDATE)
                and(C.DATE_TO  >= trunc(dWORKDATE) or C.DATE_TO is null) ) then 1
         else 0 end
    into nSAL_SHEET
    from dual;
  return nSAL_SHEET;
end;
/
show errors;

create or replace trigger T_PSPAYCARDHOUR_BUPDATE
  before update on PSPAYCARDHOUR for each row
declare
  nPSORGGRP       PKG_STD.tREF;
  sORGCODE        PSORG.CODE%type;
  sGRCODE         PSORGGRP.CODE%type;
  dWORKDATE       date;
begin
  /* проверка неизменности значений полей */
  PKG_UNCHANGE.CHECK_NE('PSPAYCARDHOUR', 'RN', :new.RN, :old.RN);
  PKG_UNCHANGE.CHECK_NE('PSPAYCARDHOUR', 'COMPANY', :new.COMPANY, :old.COMPANY);
  PKG_UNCHANGE.CHECK_NE('PSPAYCARDHOUR', 'PRN', :new.PRN, :old.PRN);

  if CMP_NUM(:new.WORKEDHOURS, :old.WORKEDHOURS) = 0 then
    select C.PSORGGRP,
         O.CODE   OCODE,
         OG.CODE  OGCODE,
         CD.WORKDATE
    into nPSORGGRP,
         sORGCODE,
         sGRCODE,
         dWORKDATE
    from PSPAYCARD    C,
         PSPAYCARDDAY CD,
         PSORG        O,
         PSORGGRP     OG
   where C.RN       = CD.PRN
     and CD.RN      = :old.PRN
     and C.PSORG    = O.RN
     and C.PSORGGRP = OG.RN;

    p_exception(abs(F_PSTS_CONFIRMED(:new.COMPANY, nPSORGGRP, dWORKDATE)-1),
      'Невозможно исправить часы в расчётной карточке по группе "'||sGRCODE||'" учереждения "'||sORGCODE||'", т.к. по этой группе уже утверждён табель.' );
    p_exception(abs(F_PSTS_SAL_SHEET(:new.COMPANY, nPSORGGRP, dWORKDATE)-1),
      'Невозможно исправить часы в расчётной карточке по группе "'||sGRCODE||'" учереждения "'||sORGCODE||'", т.к. по этой группе сформирована ведомость.' );
  end if;

  /* при изменении синхронных атрибутов заголовка триггер не активировать */
  if (CMP_NUM(:old.CRN,:new.CRN) = 0) then
    return;
  end if;

  /* регистрация события */
  if ( PKG_IUD.PROLOGUE('PSPAYCARDHOUR', 'U') ) then
    PKG_IUD.REG_RN('RN', :new.RN, :old.RN);
    PKG_IUD.REG_COMPANY('COMPANY', :new.COMPANY, :old.COMPANY);
    PKG_IUD.REG_CRN('CRN', :new.CRN, :old.CRN);
    PKG_IUD.REG_PRN('PRN', :new.PRN, :old.PRN);
    PKG_IUD.REG(1, 'HOURSTYPE', :new.HOURSTYPE, :old.HOURSTYPE);
    PKG_IUD.REG('WORKEDHOURS', :new.WORKEDHOURS, :old.WORKEDHOURS);
    PKG_IUD.EPILOGUE;
  end if;
end;
/
show errors;

create or replace trigger T_PSPAYCARDDAY_BUPDATE
  before update on PSPAYCARDDAY for each row
declare
  nPSORGGRP                              PKG_STD.tREF;
  sORGCODE                               PSORG.CODE%type;
  sGRCODE                                PSORGGRP.CODE%type;
begin
  /* проверка неизменности значений полей */
  PKG_UNCHANGE.CHECK_NE('PSPAYCARDDAY', 'RN', :new.RN, :old.RN);
  PKG_UNCHANGE.CHECK_NE('PSPAYCARDDAY', 'COMPANY', :new.COMPANY, :old.COMPANY);
  PKG_UNCHANGE.CHECK_NE('PSPAYCARDDAY', 'PRN', :new.PRN, :old.PRN);

  /* при изменении синхронных атрибутов заголовка триггер не активировать */
  if (CMP_NUM(:old.CRN,:new.CRN) = 0) then
    return;
  end if;

  if CMP_NUM(:new.DAYSTYPE, :old.DAYSTYPE) = 0 then
    select C.PSORGGRP,
           O.CODE   OCODE,
           OG.CODE  OGCODE
      into nPSORGGRP,
           sORGCODE,
           sGRCODE
      from PSPAYCARD    C,
           PSORG        O,
           PSORGGRP     OG
     where C.RN      = :old.PRN
       and C.PSORG    = O.RN
       and C.PSORGGRP = OG.RN;

    p_exception(abs(F_PSTS_CONFIRMED(:new.COMPANY, nPSORGGRP, :new.WORKDATE)-1),
      'Невозможно исправить тип дня в расчётной карточке по группе "'||sGRCODE||'" учереждения "'||sORGCODE||'", т.к. по этой группе уже утверждён табель.' );
    p_exception(abs(F_PSTS_SAL_SHEET(:new.COMPANY, nPSORGGRP, :new.WORKDATE)-1),
      'Невозможно исправить тип дня в расчётной карточке по группе "'||sGRCODE||'" учереждения "'||sORGCODE||'", т.к. по этой группе сформирована ведомость.' );
  end if;

  /* регистрация события */
  if ( PKG_IUD.PROLOGUE('PSPAYCARDDAY', 'U') ) then
    PKG_IUD.REG_RN('RN', :new.RN, :old.RN);
    PKG_IUD.REG_COMPANY('COMPANY', :new.COMPANY, :old.COMPANY);
    PKG_IUD.REG_CRN('CRN', :new.CRN, :old.CRN);
    PKG_IUD.REG_PRN('PRN', :new.PRN, :old.PRN);
    PKG_IUD.REG(1, 'WORKDATE', :new.WORKDATE, :old.WORKDATE);
    PKG_IUD.REG('DAYSTYPE', :new.DAYSTYPE, :old.DAYSTYPE);
    PKG_IUD.EPILOGUE;
  end if;
end;
/
show errors;

create or replace view V_PSTSGRP
(
  nRN,                                  -- Регистрационный номер
  nCOMPANY,                             -- Организация
  nCRN,                                 -- Каталог
  nPRN,                                 -- Родитель
  sORG_CODE,                            -- Мнемокод учреждения
  sORG_NAME,                            -- Наименование учреждения
  sCODE,                                -- Мнемокод
  sNAME,                                -- Наименование
  nGROUPKND,                            -- Категория группы
  sGROUPKND,                            -- Мнемокод категории группы
  nSHEDULE,                             -- График работы
  sSHEDULE,                             -- Мнемокод графика работы
  nPRDFORM,                             -- Форма обучения
  sPRDFORM,                             -- Мнемокод формы обучения
  dDATE_FROM,                           -- Действует с
  dDATE_TO,                             -- Действует по
  nTAB_CONF,                            -- Табель утвержден
  nIS_SAL_SHEET                         -- Ведомость
)
as
select
  T.RN,                                 -- nRN
  T.COMPANY,                            -- nCOMPANY
  T.CRN,                                -- nCRN
  T.PRN,                                -- nPRN
  M.CODE,                               -- sORG_CODE
  M.NAME,                               -- sORG_NAME
  T.CODE,                               -- sCODE
  T.NAME,                               -- sNAME
  T.GROUPKND,                           -- nGROUPKND
  P.CODE,                               -- sGROUPKND
  T.SHEDULE,                            -- nSHEDULE
  S.CODE,                               -- sSHEDULE
  T.PRDFORM,                            -- nPRDFORM
  P2.CODE,                              -- sPRDFORM
  T.DATE_FROM,                          -- dDATE_FROM
  T.DATE_TO,                            -- dDATE_TO
  F_PSTS_CONFIRMED(T.COMPANY, T.RN, GET_OPTIONS_DATE('ParentPayCards_CalcPeriod', T.COMPANY)),    -- nTAB_CONF
  F_PSTS_SAL_SHEET(T.COMPANY, T.RN, GET_OPTIONS_DATE('ParentPayCards_CalcPeriod', T.COMPANY))     -- nIS_SAL_SHEET
from
  PSORGGRP T,
  PSORG M,
  PSGRPKND P,
  SLSCHEDULE S,
  PREDFORM P2
where T.PRN = M.RN
  and T.GROUPKND = P.RN
  and T.SHEDULE = S.RN
  and T.PRDFORM = P2.RN(+)
  and exists (select null from V_USERPRIV UP where UP.CATALOG = T.CRN)
;

create or replace procedure P_PSTS_FOVDISTRIBH
(
  nCOMPANY                  in number,       -- Организация
  nIDENT                    in number,
  dBGN                      in date,
  dEND                      in date,
  sHOURSTYPE                in varchar2,
  nOUTDAYOFF                in number,
  nACCOUNT                  in number,
  nINSERT                   in number
)
as
begin
  for rREC in
  (
    select
      C.RN,
      C.CRN,
      G.CODE  GRCODE,
      OG.CODE GCODE,
      L.DOCUMENT
    from
      SELECTLIST L,
      PSORGGRP   G,
      PSORG      OG,
      PSPAYCARD  C
    where L.IDENT = nIDENT
      and G.RN = L.DOCUMENT
      and C.PSORGGRP = G.RN
      and C.DATE_FROM <= dEND
      and (C.DATE_TO >= dBGN or C.DATE_TO is null)
      and OG.RN = G.PRN
    order by G.RN
  )
  loop
    p_exception(abs(F_PSTS_CONFIRMED(nCOMPANY, rREC.DOCUMENT, dBGN)-1),
      'Невозможно разнести часы табеля по группе "'||rREC.GRCODE||'" учереждения "'||rREC.GCODE||'", т.к. этот табель уже утверждён.' );
    p_exception(abs(F_PSTS_SAL_SHEET(nCOMPANY, rREC.DOCUMENT, dBGN)-1),
      'Невозможно разнести часы табеля по группе "'||rREC.GRCODE||'" учереждения "'||rREC.GCODE||'", т.к. этот по этому табелю есть сформированная ведомость.' );
    /* фиксация начала выполнения действия */
    PKG_ENV.PROLOGUE( nCOMPANY, null, rREC.CRN, 'ParentPayCards', 'PSPAYCARD_FOVDISTRIBH', 'PSPAYCARD', rREC.RN );

    PKG_PSPAYCARDTIME.FOV_DISTRIBH( nCOMPANY, rREC.RN, dBGN, dEND, sHOURSTYPE, nOUTDAYOFF, nACCOUNT, nINSERT );

    /* фиксация окончания выполнения действия */
    PKG_ENV.EPILOGUE( nCOMPANY, null, rREC.CRN, 'ParentPayCards', 'PSPAYCARD_FOVDISTRIBH', 'PSPAYCARD', rREC.RN );
  end loop;
end;
/
show errors;

create or replace procedure P_PSTS_FOVDISTRIBD
(
  nCOMPANY      in number,      -- Организация
  nIDENT        in number,      -- Идентификатор помеченных записей
  dBGN          in date,        -- Период С
  dEND          in date,        -- Период По
  sDAYTYPE      in varchar2,    -- Мнемокод типа дня
  nOUTDAYOFF    in number       -- Исключать нерабочие дни по графику (0 - Нет, 1 - Да)
)
as
begin
  for rREC in
  (
    select
      C.RN,
      C.CRN,
      G.CODE  GRCODE,
      OG.CODE GCODE,
      L.DOCUMENT
    from
      SELECTLIST L,
      PSORGGRP   G,
      PSORG      OG,
      PSPAYCARD  C
    where L.IDENT = nIDENT
      and G.RN = L.DOCUMENT
      and C.PSORGGRP = G.RN
      and C.DATE_FROM <= dEND
      and (C.DATE_TO >= dBGN or C.DATE_TO is null)
      and OG.RN = G.PRN
    order by G.RN
  )
  loop
    p_exception(abs(F_PSTS_CONFIRMED(nCOMPANY, rREC.DOCUMENT, dBGN)-1),
      'Невозможно разнести дни табеля по группе "'||rREC.GRCODE||'" учереждения "'||rREC.GCODE||'", т.к. этот табель уже утверждён.' );
    p_exception(abs(F_PSTS_SAL_SHEET(nCOMPANY, rREC.DOCUMENT, dBGN)-1),
      'Невозможно разнести дни табеля по группе "'||rREC.GRCODE||'" учереждения "'||rREC.GCODE||'", т.к. этот по этому табелю есть сформированная ведомость.' );
    /* фиксация начала выполнения действия */
    PKG_ENV.PROLOGUE( nCOMPANY, null, rREC.CRN, 'ParentPayCards', 'PSPAYCARD_FOVDISTRIBD', 'PSPAYCARD', rREC.RN );

    PKG_PSPAYCARDTIME.FOV_DISTRIBD( nCOMPANY, rREC.RN, dBGN, dEND, sDAYTYPE, nOUTDAYOFF );

    /* фиксация окончания выполнения действия */
    PKG_ENV.EPILOGUE( nCOMPANY, null, rREC.CRN, 'ParentPayCards', 'PSPAYCARD_FOVDISTRIBD', 'PSPAYCARD', rREC.RN );
  end loop;
end;
/
show errors;

create or replace procedure P_PSTS_FOVCREATE
(
  nCOMPANY                  in number,       -- Организация
  nIDENT                    in number,
  nCALCYEAR                 in number,
  nCALCMONTH                in number
)
as
  dBGN                      date;
  dEND                      date;
begin
  dBGN := INT2DATE( 1, nCALCMONTH, nCALCYEAR );
  dEND := last_day( dBGN );

  for rREC in
  (
    select
      C.RN,
      C.CRN,
      G.CODE  GRCODE,
      OG.CODE GCODE,
      L.DOCUMENT
    from
      SELECTLIST L,
      PSORGGRP   G,
      PSORG      OG,
      PSPAYCARD  C
    where L.IDENT = nIDENT
      and G.RN = L.DOCUMENT
      and C.PSORGGRP = G.RN
      and C.DATE_FROM <= dEND
      and (C.DATE_TO >= dBGN or C.DATE_TO is null)
      and OG.RN = G.PRN
    order by G.RN
  )
  loop
    p_exception(abs(F_PSTS_CONFIRMED(nCOMPANY, rREC.DOCUMENT, dBGN)-1),
      'Невозможно сформировать табель по календарю по группе "'||rREC.GRCODE||'" учереждения "'||rREC.GCODE||'", т.к. этот табель уже утверждён.' );
    p_exception(abs(F_PSTS_SAL_SHEET(nCOMPANY, rREC.DOCUMENT, dBGN)-1),
      'Невозможно сформировать табель по календарю по группе "'||rREC.GRCODE||'" учереждения "'||rREC.GCODE||'", т.к. этот по этому табелю есть сформированная ведомость.' );
    /* фиксация начала выполнения действия */
    PKG_ENV.PROLOGUE( nCOMPANY, null, rREC.CRN, 'ParentPayCards', 'PSPAYCARD_FOVCREATE', 'PSPAYCARD', rREC.RN );

    PKG_PSPAYCARDTIME.FOV_CREATE( nCOMPANY, rREC.RN, dBGN, dEND );

    /* фиксация окончания выполнения действия */
    PKG_ENV.EPILOGUE( nCOMPANY, null, rREC.CRN, 'ParentPayCards', 'PSPAYCARD_FOVCREATE', 'PSPAYCARD', rREC.RN );
  end loop;
end;
/
show errors;

create or replace procedure P_PSPAYCARD_PSTS_UNCONFIRM
(
  nCOMPANY                  in number,
  nIDENT                    in number,       -- помеченные записи
  nUNCONFIRMED             out number        -- записей утверждено
)
as
  dCALCPER                     date          :=GET_OPTIONS_DATE('ParentPayCards_CalcPeriod', nCOMPANY);
  nALL_CONF                    number;
  nOLD_CRN                     number        :=null;
  nOLD_C_RN                    number        :=null;
begin
  nUNCONFIRMED:= 0;
  -- цикл по группам
  for rRECC in
  (
     select SL.DOCUMENT,
            G.CODE        GCODE,
            GR.CODE       GRCODE
       from SELECTLIST    SL,
            PSORGGRP      GR,
            PSORG         G
      where SL.IDENT    = nIDENT
        and SL.DOCUMENT = GR.RN
        and G.RN        = GR.PRN
  )
  loop
    p_exception(F_PSTS_CONFIRMED(nCOMPANY, rRECC.DOCUMENT, dCALCPER),
      'Невозможно снять утверждение с табеля по группе "'||rRECC.GRCODE||'" учереждения "'||rRECC.GCODE||'", т.к. этот табель еще не утверждён.' );
    p_exception(abs(F_PSTS_SAL_SHEET(nCOMPANY, rRECC.DOCUMENT, dCALCPER)-1),
      'Невозможно снять утверждение с табеля по группе "'||rRECC.GRCODE||'" учереждения "'||rRECC.GCODE||'", т.к. этот по этому табелю есть сформированная ведомость.' );
    -- цикл по РК
    for rREC in
    (
       select C.RN           C_RN,
              C.CRN,
              PR.RN          PR_RN,
              C.PSORGGRP
        from  PSPAYCARD      C,
              PSPAYCARDPRD   PR
        where C.PSORGGRP   = rRECC.DOCUMENT
          and C.RN         = PR.PRN
          and PR.TAB_CONF  = 1
          and C.DATE_FROM <= last_day(dCALCPER)
          and(C.DATE_TO   >= trunc(dCALCPER, 'MM') or C.DATE_TO is null)
          and PR.YEAR      = extract(year from dCALCPER)
          and PR.MONTH     = extract(month from dCALCPER)
    )
    loop
      if CMP_NUM(rREC.C_RN, nOLD_C_RN)=0 then
        if nOLD_C_RN is not null then
          -- фиксация окончания выполнения действия
          PKG_ENV.EPILOGUE( nCOMPANY, null, nOLD_CRN, 'ParentPayCards', 'PSPAYCARD_PSTS_UNCONFIRM', 'PSPAYCARD', nOLD_C_RN );
        end if;
        -- фиксация начала выполнения действия
        PKG_ENV.PROLOGUE( nCOMPANY, null, rREC.CRN, 'ParentPayCards', 'PSPAYCARD_PSTS_UNCONFIRM', 'PSPAYCARD', rREC.C_RN );
      end if;

      update PSPAYCARDPRD PR
         set TAB_CONF = 0
       where PR.RN = rREC.PR_RN;
      nUNCONFIRMED:= nUNCONFIRMED+1;

      nOLD_CRN:=  rREC.CRN;
      nOLD_C_RN:=  rREC.C_RN;
    end loop;
    if nOLD_C_RN is not null then
      -- фиксация окончания выполнения действия
      PKG_ENV.EPILOGUE( nCOMPANY, null, nOLD_CRN, 'ParentPayCards', 'PSPAYCARD_PSTS_UNCONFIRM', 'PSPAYCARD', nOLD_C_RN );
    end if;
  end loop;
end;
/
show errors;

create or replace procedure P_PSPAYCARD_PSTS_CONFIRM
(
  nCOMPANY                  in number,
  nIDENT                    in number,      -- помеченные записи
  nCONFIRMED               out number       -- записей утверждено
)
as
  dCALCPER                     date         := GET_OPTIONS_DATE('ParentPayCards_CalcPeriod', nCOMPANY);
  nOLD_CRN                     number       :=null;
  nOLD_C_RN                    number       :=null;
begin
  nCONFIRMED:= 0;
  -- цикл по группам
  for rRECC in
  (
     select SL.DOCUMENT,
            G.CODE        GCODE,
            GR.CODE       GRCODE
       from SELECTLIST    SL,
            PSORGGRP      GR,
            PSORG         G
      where SL.IDENT    = nIDENT
        and SL.DOCUMENT = GR.RN
        and G.RN        = GR.PRN
  )
  loop
    p_exception(abs(F_PSTS_CONFIRMED(nCOMPANY, rRECC.DOCUMENT, dCALCPER)-1),
      'Невозможно утвердить табель по группе "'||rRECC.GRCODE||'" учереждения "'||rRECC.GCODE||'", т.к. этот табель уже утверждён.' );
    -- цикл по РК
    for rREC in
    (
       select C.RN    C_RN,
              C.CRN,
              PR.RN   PR_RN,
              PR.TAB_CONF
        from  PSPAYCARD    C,
              PSPAYCARDPRD PR
        where rRECC.DOCUMENT = C.PSORGGRP
          and C.RN           = PR.PRN (+)
          and C.DATE_FROM   <= last_day(dCALCPER)
          and(C.DATE_TO     >= trunc(dCALCPER, 'MM') or C.DATE_TO is null)
          and(extract(year  from dCALCPER)  = PR.YEAR (+)
          and extract(month from dCALCPER) = PR.MONTH (+))
    )
    loop
      if CMP_NUM(rREC.C_RN, nOLD_C_RN)=0 then
        if nOLD_C_RN is not null then
          -- фиксация окончания выполнения действия
          PKG_ENV.EPILOGUE( nCOMPANY, null, nOLD_CRN, 'ParentPayCards', 'PSPAYCARD_PSTS_CONFIRM', 'PSPAYCARD', nOLD_C_RN );
        end if;
        -- фиксация начала выполнения действия
        PKG_ENV.PROLOGUE( nCOMPANY, null, rREC.CRN, 'ParentPayCards', 'PSPAYCARD_PSTS_CONFIRM', 'PSPAYCARD', rREC.C_RN );
      end if;

      if rREC.PR_RN is null then
        P_PSPAYCARDPRD_BASE_INSERT(nCOMPANY, rREC.C_RN, extract(year from dCALCPER), extract(month from dCALCPER), 1, rREC.PR_RN);
        nCONFIRMED:= nCONFIRMED+1;
      elsif rREC.PR_RN is not null and rREC.TAB_CONF=0 then
        update PSPAYCARDPRD PR
           set TAB_CONF = 1
         where PR.RN = rREC.PR_RN;
        nCONFIRMED:= nCONFIRMED+1;
      end if;
      nOLD_CRN:=  rREC.CRN;
      nOLD_C_RN:=  rREC.C_RN;
    end loop;
    if nOLD_C_RN is not null then
      -- фиксация окончания выполнения действия
      PKG_ENV.EPILOGUE( nCOMPANY, null, nOLD_CRN, 'ParentPayCards', 'PSPAYCARD_PSTS_CONFIRM', 'PSPAYCARD', nOLD_C_RN );
    end if;
  end loop;
end;
/
show errors;

create or replace trigger T_PSPAYCARDHOUR_BINSERT
  before insert on PSPAYCARDHOUR for each row
declare
  nPSORGGRP       PKG_STD.tREF;
  sORGCODE        PSORG.CODE%type;
  sGRCODE         PSORGGRP.CODE%type;
  dWORKDATE       date;
begin
  /* считывание параметров записи master-таблицы */
  select CD.COMPANY,
         CD.CRN,
         C.PSORGGRP,
         O.CODE   OCODE,
         OG.CODE  OGCODE,
         CD.WORKDATE
    into :new.COMPANY,
         :new.CRN,
         nPSORGGRP,
         sORGCODE,
         sGRCODE,
         dWORKDATE
    from PSPAYCARDDAY CD,
         PSPAYCARD    C,
         PSORG        O,
         PSORGGRP     OG
   where CD.RN      = :new.PRN
     and C.RN       = CD.PRN
     and C.PSORG    = O.RN
     and C.PSORGGRP = OG.RN;

  if :new.WORKEDHOURS!= 0 then
    P_EXCEPTION(abs(F_PSTS_CONFIRMED(:new.COMPANY, nPSORGGRP, dWORKDATE)-1),
      'Невозможно добавить часы в расчётную карточку по группе "'||sGRCODE||'" учереждения "'||sORGCODE||'", т.к. по этой группе уже утверждён табель.' );
    P_EXCEPTION(abs(F_PSTS_SAL_SHEET(:new.COMPANY, nPSORGGRP, dWORKDATE)-1),
      'Невозможно добавить часы в расчётную карточку по группе "'||sGRCODE||'" учереждения "'||sORGCODE||'", т.к. по этой группе сформирована ведомость.' );
  end if;

  /* регистрация события */
  if ( PKG_IUD.PROLOGUE('PSPAYCARDHOUR', 'I') ) then
    PKG_IUD.REG_RN('RN', :new.RN);
    PKG_IUD.REG_COMPANY('COMPANY', :new.COMPANY);
    PKG_IUD.REG_CRN('CRN', :new.CRN);
    PKG_IUD.REG_PRN('PRN', :new.PRN);
    PKG_IUD.REG(1, 'HOURSTYPE', :new.HOURSTYPE);
    PKG_IUD.REG('WORKEDHOURS', :new.WORKEDHOURS);
    PKG_IUD.EPILOGUE;
  end if;
end;
/
show errors;

create or replace function F_PAN_PSORGGRP_CHECK_CLOSE
(
  nRN             in number,            -- RN группы учреждения
  dWORKDATE       in date               -- Дата табеля
)
return number
as
  nCOMPANY        PKG_STD.tREF;
  dCURRENT_PERIOD date;
  nRESULT         number(1);

begin
  -- считывание организации группы учреждения
  begin
    select G.COMPANY
      into nCOMPANY
      from PSORGGRP G
     where G.RN = nRN;
  exception
    when NO_DATA_FOUND then
      return null;
  end;

  -- поиск закрытого периода группы
  begin
    select P.PERIOD
      into dCURRENT_PERIOD
      from PAN_PSORGGRPCP P
     where P.PRN = nRN
       and P.PERIOD = trunc(dWORKDATE, 'month');

     nRESULT := 1;
  exception
    when NO_DATA_FOUND then
       nRESULT := 0;
  end;

  return nRESULT;
end;
/
show errors;

create or replace procedure P_PAN_PSTSBRD_CHECK
(
  nPSPAYCARD      in number,            -- RN расчётной карточки
  dWORKDATE       in date               -- Дата периода
)
as
  nPSORGGRP       PKG_STD.tREF;
  sGROUP          PKG_STD.tSTRING;
  sORG            PKG_STD.tSTRING;

begin
  -- считывание организации и группы расчётной карточки
  begin
    select PC.PSORGGRP,
           G.CODE,
           O.CODE
      into nPSORGGRP,
           sGROUP,
           sORG
      from PSPAYCARD PC,
           PSORGGRP G,
           PSORG O
     where PC.RN = nPSPAYCARD
       and PC.PSORGGRP = G.RN
       and G.PRN = O.RN;
  exception
    when NO_DATA_FOUND then
      PKG_MSG.RECORD_NOT_FOUND(nPSPAYCARD, 'ParentPayCards');
  end;

  -- проверка закрытия текущего расчётного периода
  if F_PAN_PSORGGRP_CHECK_CLOSE(nPSORGGRP, dWORKDATE) = 1 then
    P_EXCEPTION(0, 'Табель группы "%s" учреждения "%s" закрыт для исправления в периоде %s. Для исправления требуется открыть табель.',
      to_char(dWORKDATE, 'mm/yyyy'));
  end if;
end;
/
show errors;

create or replace triggeR T_PAN_PSPAYCARDDAY_BUPDATE
  before update on PSPAYCARDDAY for each row
begin
  if cmp_dat( :new.WORKDATE, :old.WORKDATE ) = 0 then
    P_PAN_PSTSBRD_CHECK(:new.PRN, :new.WORKDATE);
  end if;
end;
/
show errors;

create or replace trigger T_PAN_PSPAYCARDHOUR_BISNERT
  before insert on PSPAYCARDHOUR for each row
declare
  nPSPAYCARD      PKG_STD.tREF;
  dWORKDATE       date;
begin
  select D.PRN,
         D.WORKDATE
    into nPSPAYCARD,
         dWORKDATE
    from PSPAYCARDDAY D
   where D.RN = :new.PRN;

  P_PAN_PSTSBRD_CHECK(nPSPAYCARD, dWORKDATE);
end;
/
show errors;

create or replace trigger T_PAN_PSPAYCARDHOUR_BUPDATE
  before update on PSPAYCARDHOUR for each row
declare
  nPSPAYCARD      PKG_STD.tREF;
  dWORKDATE       date;
begin
  if cmp_num(:new.WORKEDHOURS, :old.WORKEDHOURS) = 0 then
    select D.PRN,
           D.WORKDATE
      into nPSPAYCARD,
           dWORKDATE
      from PSPAYCARDDAY D
     where D.RN = :new.PRN;

    P_PAN_PSTSBRD_CHECK(nPSPAYCARD, dWORKDATE);
  end if;
end;
/
show errors;

create or replace trigger T_PAN_PSPAYCARDDAY_BINSERT
  before insert on PSPAYCARDDAY for each row
begin
  P_PAN_PSTSBRD_CHECK(:new.PRN, :new.WORKDATE);
end;
/
show errors;

create or replace procedure UDO_P_TIMESHEET_RECEIVE
-- Получение табеля посещаемости из CSV файла
(
  nCOMPANY        in number,            -- Организация
  cDATA           in clob,              -- Табель посещаемости группы в формате CSV
  sMESSAGE        out varchar2          -- Результат
)
as
  sDAY_TYPE_NU    PKG_STD.tSTRING := 'НУ';
  sLINE           varchar2(32767);
  nLINE_NUMBER    binary_integer := 1;
  dPERIOD         date;
  sAGNFAMILYNAME  PKG_STD.tSTRING;
  sAGNFIRSTNAME   PKG_STD.tSTRING;
  SAGNLASTNAME    PKG_STD.tSTRING;
  dAGNBURN        date;
  sPHONE          PKG_STD.tSTRING;
  sPHONE2         PKG_STD.tSTRING;
  dDATE_FROM      date;
  dDATE_TO        date;
  dPERIOD_BEGIN   date;
  nORG_RN         PKG_STD.tREF;
  nGROUP_RN       PKG_STD.tREF;
  nPAYCARD_RN     PKG_STD.tREF;
  nERROR_COUNT    binary_integer := 0;
  nSUCCESS_COUNT  binary_integer := 0;
  nDAYS_IN_MONTH  binary_integer;
  nMONTH          binary_integer;
  nYEAR           binary_integer;
  nHOURSTYPE      PKG_STD.tREF;
  sHOURSTYPE      PKG_STD.tSTRING;
  dDATE           date;
  nPAYCARDDAY     PKG_STD.tREF;
  nPAYCARDHOUR    PKG_STD.tREF;
  nWORKEDHOURS    PKG_STD.tSUMM;
  nHOURS_FACT     PKG_STD.tSUMM;
  sORG_CODE       PKG_STD.tSTRING;
  sORG_INN        PKG_STD.tSTRING;
  sGROUP_CODE     PKG_STD.tSTRING;
  sPERION         PKG_STD.tSTRING;
  l_amount        pls_integer := 1;
  l_length        pls_integer;
  token           varchar2(4000);
  sobch           varchar2(4000);
  sERROR          PKG_STD.tSTRING;
  nDAYSTYPE_NU    PKG_STD.tREF;
  sDAYSTYPE_CODE  PKG_STD.tSTRING;

  -- Обработка строки файла
  procedure PERFORM_LINE
  (
    nLINE_NUMBER  in number,
    sSOURCE_LINE  in varchar2
  )
  as
    sLINE         PKG_STD.tSTRING := sSOURCE_LINE;
    sDAY_VALUE    PKG_STD.tSTRING;
    nDAYSTYPE     PKG_STD.tREF;
  begin
    -- Удаление лишних ";" в конце строки
    while substr(sLINE, -1) = ';' loop
      sLINE          := substr(sLINE, 1, length(sLINE) - 1);
    end loop;
    sLINE := sLINE||';';

    -- Текущий период
    if nLINE_NUMBER = 1 then
      sPERION        := UDO_F_GET_LIST_ITEM(sLINE, 1, ';');
      dPERIOD        := UDO_F_S2D(sPERION);
      dPERIOD_BEGIN  := trunc(dPERIOD, 'month');
      nDAYS_IN_MONTH := extract(day from last_day(dPERIOD));
      nMONTH         := extract(month from dPERIOD);
      nYEAR          := extract(year from dPERIOD);

    -- Организация
    elsif nLINE_NUMBER = 2 then
      sORG_CODE := UDO_F_GET_LIST_ITEM(sLINE, 1, ';');
      sORG_INN := UDO_F_GET_LIST_ITEM(sLINE, 2, ';');

      begin
        select O.RN
          into nORG_RN
          from PSORG O
         where O.COMPANY = nCOMPANY
           and O.CODE = sORG_CODE;
      exception
        when NO_DATA_FOUND then
          P_EXCEPTION(0, 'Организация "%s" не найдена', sORG_CODE);
      end;

    -- Группа
    elsif nLINE_NUMBER = 3 then
      sGROUP_CODE := UDO_F_GET_LIST_ITEM(sLINE, 1, ';');

      begin
        select G.RN
          into nGROUP_RN
          from PSORGGRP G
         where G.PRN = nORG_RN
           and G.CODE = sGROUP_CODE;
      exception
        when NO_DATA_FOUND then
          P_EXCEPTION(0, 'Группа "%s" в организации "%s" не найдена', sGROUP_CODE, sORG_CODE);
      end;

    -- Посещаемость персоны в группе
    elsif nLINE_NUMBER > 4 and trim(sLINE) is not null then
      sAGNFAMILYNAME := UDO_F_GET_LIST_ITEM(sLINE, 1, ';');
      sAGNFIRSTNAME  := UDO_F_GET_LIST_ITEM(sLINE, 2, ';');
      SAGNLASTNAME   := UDO_F_GET_LIST_ITEM(sLINE, 3, ';');
      dAGNBURN       := UDO_F_S2D(UDO_F_GET_LIST_ITEM(sLINE, 4, ';'));
      sPHONE         := UDO_F_GET_LIST_ITEM(sLINE, 5, ';');
      sPHONE2        := UDO_F_GET_LIST_ITEM(sLINE, 6, ';');
      dDATE_FROM     := UDO_F_S2D(UDO_F_GET_LIST_ITEM(sLINE, 7, ';'));
      dDATE_TO       := UDO_F_S2D(UDO_F_GET_LIST_ITEM(sLINE, 8, ';'));

      -- Поиск расчётной карточки
      begin
        select PC.RN
          into nPAYCARD_RN
          from PSPAYCARD PC,
               PSPERSCARD C,
               AGNLIST A
         where PC.PSORGGRP = nGROUP_RN
           and PC.DATE_FROM <= last_day(dPERIOD)
           and (PC.DATE_TO is null or PC.DATE_TO >= dPERIOD_BEGIN)
           and PC.PERSCARD = C.RN
           and C.AGENT = A.RN
           and trim(A.AGNFAMILYNAME) = trim(sAGNFAMILYNAME)
           and trim(A.AGNFIRSTNAME) = trim(sAGNFIRSTNAME)
           and cmp_vc2(trim(A.AGNLASTNAME), trim(sAGNLASTNAME)) = 1
           and cmp_dat(A.AGNBURN, dAGNBURN) = 1;
      exception
        when NO_DATA_FOUND then
          begin
            select PC.RN
              into nPAYCARD_RN
              from PSPAYCARD PC,
                   PSPERSCARD C,
                   AGNLIST A
             where PC.PSORGGRP = nGROUP_RN
               and PC.DATE_FROM <= last_day(dPERIOD)
               and (PC.DATE_TO is null or PC.DATE_TO >= dPERIOD_BEGIN)
               and PC.PERSCARD = C.RN
               and C.AGENT = A.RN
               and trim(A.AGNFAMILYNAME) = trim(sAGNFAMILYNAME)
               and trim(A.AGNFIRSTNAME) = trim(sAGNFIRSTNAME);
          exception
            when NO_DATA_FOUND then
              nERROR_COUNT := nERROR_COUNT + 1;
              sERROR := substr(sERROR||FORMAT_TEXT('%s. %s %s не найден(а)'||chr(10),
                to_char(nERROR_COUNT), sAGNFAMILYNAME, sAGNFIRSTNAME), 1, 4000);
              return;
          end;
      end;

      -- Проверка закрытия периода
      P_PAN_PSTSBRD_CHECK(nPAYCARD_RN, dPERIOD_BEGIN);

      -- Посещаемость по дням
      for d in 1 .. nDAYS_IN_MONTH loop
        sDAY_VALUE := UDO_F_GET_LIST_ITEM(sLINE, 8 + d, ';');
        nHOURS_FACT := nvl(UDO_F_S2N(sDAY_VALUE), 0);
        dDATE := int2date(d, nMONTH, nYEAR);

        if sDAY_VALUE = sDAY_TYPE_NU then
          nDAYSTYPE := nDAYSTYPE_NU;
          nHOURS_FACT := 0;
        else
          nDAYSTYPE := null;
          nHOURS_FACT := nvl(UDO_F_S2N(sDAY_VALUE), 0);
        end if;

        if nHOURS_FACT > 0 or nDAYSTYPE is not null then
          nPAYCARDDAY := PKG_PSPAYCARDTIME.CREATE_DAY(nCOMPANY, nPAYCARD_RN, dDATE, nDAYSTYPE, 1);
          begin
            select H.RN,
                   H.WORKEDHOURS
              into nPAYCARDHOUR,
                   nWORKEDHOURS
              from PSPAYCARDHOUR H
             where H.PRN = nPAYCARDDAY
               and H.HOURSTYPE = nHOURSTYPE;

            if nWORKEDHOURS != nHOURS_FACT then
              update PSPAYCARDHOUR H
                 set H.WORKEDHOURS = nHOURS_FACT
               where H.PRN = nPAYCARDDAY
                 and H.HOURSTYPE = nHOURSTYPE;
            end if;
          exception
            when NO_DATA_FOUND then
              begin
                P_PSPAYCARDHOUR_BASE_INSERT
                (
                  nCOMPANY     => nCOMPANY,     -- Организация
                  nPRN         => nPAYCARDDAY,  -- RN дня
                  nHOURSTYPE   => nHOURSTYPE,   -- Тип часа
                  nWORKEDHOURS => nHOURS_FACT,  -- Количество часов
                  nRN          => nPAYCARDHOUR  -- RN часа
                );
              exception
                when OTHERS then
                  P_EXCEPTION(0, 'Ошибка загрузки табеля в Парус: '||ERROR_TEXT);
              end;
          end;
        else
          delete
            from PSPAYCARDDAY D
           where D.PRN = nPAYCARD_RN
             and D.WORKDATE = dDATE;
        end if;
      end loop;
      nSUCCESS_COUNT := nSUCCESS_COUNT + 1;
    end if;
  end;

begin
  -- Тип часа
  begin
    select T.RN,
           T.CODE
      into nHOURSTYPE,
           sHOURSTYPE
      from SL_HOURS_TYPES T
     where T.BASE_SIGN = 1
       and substr(upper(T.SHORT_CODE), 1, 1) = 'Д';
  exception
    when NO_DATA_FOUND then
      P_EXCEPTION(0, 'Основной тип часа с кодом Д не найден');
  end;

  -- Тип дня
  FIND_SLDAYSTYPE_SHORTCODE(0, 0, nCOMPANY, sDAY_TYPE_NU, sDAYSTYPE_CODE, nDAYSTYPE_NU);

  -- Обработка файла
  begin
    l_length := dbms_lob.getlength(cDATA);
    for i in 1..l_length loop
      DBMS_LOB.READ
      (
        LOB_LOC => cDATA,
        AMOUNT => l_amount,
        OFFSET => i,
        BUFFER => token
      );
      if token = chr(10) then
        PERFORM_LINE(nLINE_NUMBER, sobch);
        nLINE_NUMBER := nLINE_NUMBER + 1;
        sobch:= null;
      elsif i = l_length then
        sobch:= sobch||token;
        PERFORM_LINE(nLINE_NUMBER, sobch);
      else
        sobch:= sobch||token;
      end if;
    end loop;

    if nERROR_COUNT = 0 then
      sMESSAGE := 'Табель посещаемости успешно загружен в Парус';
    else
      sMESSAGE := substr(FORMAT_TEXT('Загружено персон: %sОшибки загрузки:%s', to_char(nSUCCESS_COUNT)||chr(10), chr(10)||sERROR), 1, 4000);
    end if;
  exception
    when OTHERS then
      sMESSAGE := ERROR_TEXT;
  end;
end;
/
show errors;
create or replace public synonym UDO_P_TIMESHEET_RECEIVE for UDO_P_TIMESHEET_RECEIVE;
grant execute on UDO_P_TIMESHEET_RECEIVE to public;

create or replace procedure UDO_P_PSORGGRP_LOAD
-- Загрузка табеля посещаемости группы из CSV файла
(
  nCOMPANY        in number,            -- Организация
  nIDENT          in number,            -- Идентификатор отмеченной группы
  sMESSAGE        out varchar2          -- Результат
)
as
  cDATA           clob;
begin
  -- Загрузка файла из буфера
  begin
    select B.DATA
      into cDATA
      from FILE_BUFFER B
     where B.IDENT = nIDENT;
  exception
    when NO_DATA_FOUND then
      P_EXCEPTION(0, 'Должен быть загружен CSV файл в файловый буфер.');
    when TOO_MANY_ROWS then
      P_EXCEPTION(0, 'Должен быть загружен только один CSV файл в файловый буфер.');
  end;

  -- Обработка файла
  UDO_P_TIMESHEET_RECEIVE(nCOMPANY, cDATA, sMESSAGE);
end;
/
show errors;
create or replace public synonym UDO_P_PSORGGRP_LOAD for UDO_P_PSORGGRP_LOAD;
grant execute on UDO_P_PSORGGRP_LOAD to public;

create or replace procedure UDO_P_TIMESHEET_SEND
-- Отправка табеля посещаемости в формате CSV
(
  nORG_RN         in number,            -- RN организации
  sGROUP          in varchar2,          -- Мнемокод группы
  dPERIOD         in date,              -- Период табеля посещаемости
  sFILENAME       out varchar2,         -- Имя файла CSV
  cDATA           out clob              -- Табель посещаемости группы в формате CSV
)
as
  dPERIOD_        date := nvl(dPERIOD, trunc(sysdate));
  nDAYS_IN_MONTH  binary_integer := D_DAY(last_day(dPERIOD_));
  nMONTH          binary_integer := D_MONTH(dPERIOD_);
  nYEAR           binary_integer := D_YEAR(dPERIOD_);
  sPERIOD         PKG_STD.tSTRING := upper(F_GET_MONTH(nMONTH))||' '||nYEAR;
  sTEXT           PKG_STD.tSTRING;
  CR              varchar2(1) := chr(10);
  nWORKEDHOURS    PKG_STD.tSUMM;
  sDAYSTYPE       PKG_STD.tSTRING;
begin
  -- Создание буфера
  DBMS_LOB.CREATETEMPORARY(cDATA, true);

  -- Период, организация и группа
  for cur in
  (
    select O.CODE ORG_CODE,
           A.AGNIDNUMB,
           G.CODE GROUP_CODE,
           UDO_F_SLSCHEDULE_MNEMOCODE(G.SHEDULE) SLSCHEDULE_CODE,
           decode(lower(K.CODE), 'ясли', 1, 2) MEALS,
           G.RN GROUP_RN
      from PSORG O,
           AGNLIST A,
           PSORGGRP G,
           PSGRPKND K
     where O.RN = nORG_RN
       and O.AGENT = A.RN
       and G.PRN = O.RN
       and G.CODE = sGROUP
       and G.GROUPKND = K.RN
  )
  loop
    -- Заголовок
    sFILENAME := UDO_F_MAKE_FILE_NAME(cur.ORG_CODE||'_'||cur.GROUP_CODE||'_'||sPERIOD||'_Парус', 'csv');
    sTEXT := sPERIOD||';'||CR||
             cur.ORG_CODE||';'||cur.AGNIDNUMB||';'||CR||
             cur.GROUP_CODE||';'||cur.SLSCHEDULE_CODE||';'||cur.MEALS||';'||CR;
    DBMS_LOB.WRITEAPPEND(cDATA, length(sTEXT), sTEXT);
    sTEXT := 'Фамилия;Имя;Отчество;Дата рождения;Телефон 1;Телефон 2;Дата поступления;Дата выбытия;';
    for d in 1..nDAYS_IN_MONTH loop
      sTEXT := sTEXT||d||';';
    end loop;
    sTEXT := sTEXT||CR;
    DBMS_LOB.WRITEAPPEND(cDATA, length(sTEXT), sTEXT);

    -- Табель посещаемости
    for card in
    (
      select A1.AGNFAMILYNAME,
             A1.AGNFIRSTNAME,
             A1.AGNLASTNAME,
             to_char(A1.AGNBURN, 'dd.mm.yyyy') AGNBURN,
             nvl(A1.PHONE, A2.PHONE) PHONE,
             nvl(A1.PHONE2, A2.PHONE2) PHONE2,
             to_char(PC.DATE_FROM, 'dd.mm.yyyy') DATE_FROM,
             to_char(PC.DATE_TO, 'dd.mm.yyyy') DATE_TO,
             PC.RN PAYCARD_RN
        from PSPAYCARD PC,
             PSPERSCARD C,
             AGNLIST A1,
             AGNLIST A2
       where PC.PSORGGRP = cur.GROUP_RN
         and PC.DATE_FROM <= last_day(dPERIOD_)
         and (PC.DATE_TO is null or PC.DATE_TO >= trunc(dPERIOD_, 'month'))
         and PC.PERSCARD = C.RN
         and C.AGENT = A1.RN
         and C.PAYER = A2.RN(+)
       order by
             A1.AGNFAMILYNAME,
             A1.AGNFIRSTNAME,
             A1.AGNLASTNAME,
             A1.AGNBURN
    )
    loop
      -- Персона в группе
      sTEXT := card.AGNFAMILYNAME||';'||card.AGNFIRSTNAME||';'||card.AGNLASTNAME||';'||card.AGNBURN||';'||
               card.PHONE||';'||card.PHONE2||';'||card.DATE_FROM||';'||card.DATE_TO||';';

      -- Посещаемость по дням
      for d in 1 .. nDAYS_IN_MONTH loop
        begin
          select H.WORKEDHOURS,
                 DT.SHORT_CODE
            into nWORKEDHOURS,
                 sDAYSTYPE
            from PSPAYCARDDAY D,
                 SLDAYSTYPE DT,
                 PSPAYCARDHOUR H,
                 SL_HOURS_TYPES T
           where D.PRN = card.PAYCARD_RN
             and D.DAYSTYPE = DT.RN(+)
             and D.WORKDATE = int2date(d, nMONTH, nYEAR)
             and D.RN = H.PRN
             and H.HOURSTYPE = T.RN
             and T.BASE_SIGN = 1
             and substr(upper(T.SHORT_CODE), 1, 1) = 'Д';

          if sDAYSTYPE = 'Б' then
            sDAYSTYPE := 'НУ';
          end if;

          sTEXT := sTEXT||nvl(sDAYSTYPE, nWORKEDHOURS);
        exception
          when NO_DATA_FOUND then
            null;
        end;
        sTEXT := sTEXT||';';
      end loop;
      sTEXT := sTEXT||CR;
      DBMS_LOB.WRITEAPPEND(cDATA, length(sTEXT), sTEXT);
    end loop;
  end loop;
end;
/
show errors;
create or replace public synonym UDO_P_TIMESHEET_SEND for UDO_P_TIMESHEET_SEND;
grant execute on UDO_P_TIMESHEET_SEND to public;

create or replace procedure UDO_P_PSORGGRP_UNLOAD
-- Отправка табеля посещаемости в формате CSV
(
  nIDENT          in number,            -- Идентификатор отмеченной группы
  dPERIOD         in date               -- Период табеля посещаемости
)
as
  cDATA           clob;
  sFILENAME       PKG_STD.tSTRING;
  nCOUNT          binary_integer;
begin
  -- Создание буфера
  DBMS_LOB.CREATETEMPORARY(cDATA, true);

  -- Проверка количества отмеченных групп
  select count(*)
    into nCOUNT
    from SELECTLIST SL
   where SL.IDENT = nIDENT;

  if nCOUNT != 1 then
    P_EXCEPTION(0, 'Должна быть отмечена одна группа.');
  end if;

  -- Выгрузка табеля посещаемости группы в формате CSV
  for cur in
  (
    select G.PRN ORG_RN,
           G.CODE GROUP_CODE
      from SELECTLIST SL,
           PSORGGRP G
     where SL.IDENT = nIDENT
       and SL.DOCUMENT = G.RN
  )
  loop
    UDO_P_TIMESHEET_SEND
    (
      nORG_RN         => cur.ORG_RN,        -- RN организации
      sGROUP          => cur.GROUP_CODE,    -- Мнемокод группы
      dPERIOD         => dPERIOD,           -- Период табеля посещаемости
      sFILENAME       => sFILENAME,         -- Имя файла CSV
      cDATA           => cDATA              -- Табель посещаемости группы в формате CSV
    );
  end loop;

  -- Запись буфера
  P_FILE_BUFFER_INSERT(nIDENT, sFILENAME, cDATA, null);

  -- Освобождение буфера
  DBMS_LOB.FREETEMPORARY(cDATA);
end;
/
show errors;
create or replace public synonym UDO_P_PSORGGRP_UNLOAD for UDO_P_PSORGGRP_UNLOAD;
grant execute on UDO_P_PSORGGRP_UNLOAD to public;

create or replace procedure UDO_P_PSORG_GET_GROUPS
-- Получение списка групп учреждения
(
  nORG_RN         in number,            -- RN учреждения
  sGROUPS         out varchar2          -- Список мнемокодов групп через ";"
)
as
begin
  for cur in
  (
    select G.CODE
      from PSORGGRP G
     where G.PRN = nORG_RN
       and G.DATE_TO is null
     order by 1
  )
  loop
    sGROUPS := sGROUPS||cur.CODE||';';
  end loop;
  sGROUPS := substr(sGROUPS, 1, length(sGROUPS)-1);
end;
/
show errors;
create or replace public synonym UDO_P_PSORG_GET_GROUPS for UDO_P_PSORG_GET_GROUPS;
grant execute on UDO_P_PSORG_GET_GROUPS to public;

create or replace procedure UDO_P_GET_PSORGS
-- Получение списка учреждений по ИНН
(
  sINN            in varchar2,          -- ИНН учреждения
  sORGS_JSON      out varchar2          -- Список учреждений в формате JSON
)
as
begin
  sORGS_JSON := '[';
  for cur in
  (
    select O.RN,
           replace(O.CODE, '"', '\"') CODE,
           replace(O.NAME, '"', '\"') NAME,
           O.COMPANY
      from PSORG O,
           AGNLIST A,
           COMPANIES C,
           AGNLIST CA
     where O.AGENT = A.RN
       and A.AGNIDNUMB = sINN
       and O.COMPANY = C.RN
       and C.AGENT = CA.RN
       and exists
           (
             select *
               from PSORGGRP G
              where G.PRN = O.RN
                and G.DATE_TO is null
           )
  )
  loop
    sORGS_JSON := sORGS_JSON||FORMAT_TEXT(
      '{"org_rn": %s, "org_code": "%s", "org_name": "%s", "org_inn": "%s", "company_rn": %s}, ',
      cur.RN, cur.CODE, cur.NAME, sINN, cur.COMPANY);
  end loop;
  if sORGS_JSON = '[' then
    sORGS_JSON := '[]';
  else
    sORGS_JSON := substr(sORGS_JSON, 1, length(sORGS_JSON)-2)||']';
  end if;
end;
/
show errors;
create or replace public synonym UDO_P_GET_PSORGS for UDO_P_GET_PSORGS;
grant execute on UDO_P_GET_PSORGS to public;

create or replace procedure UDO_P_GET_PSORG
-- Получение учреждения по ИНН и мнемемокоду группы
(
  sINN            in varchar2,          -- ИНН учреждения
  sGROUP          in varchar2,          -- Мнемокод группы
  sORG_JSON       out varchar2          -- Список учреждений в формате JSON
)
as
begin
  for cur in
  (
    select O.RN,
           replace(O.CODE, '"', '\"') CODE,
           replace(O.NAME, '"', '\"') NAME,
           O.COMPANY
      from PSORG O,
           AGNLIST A,
           COMPANIES C,
           AGNLIST CA
     where O.AGENT = A.RN
       and A.AGNIDNUMB = sINN
       and O.COMPANY = C.RN
       and C.AGENT = CA.RN
       and exists
           (
             select *
               from PSORGGRP G
              where G.PRN = O.RN
                and G.CODE = sGROUP
           )
  )
  loop
    sORG_JSON := sORG_JSON||FORMAT_TEXT(
      '{"org_rn": %s, "org_code": "%s", "org_name": "%s", "org_inn": "%s", "company_rn": %s}, ',
      cur.RN, cur.CODE, cur.NAME, sINN, cur.COMPANY);
  end loop;
  sORG_JSON := substr(sORG_JSON, 1, length(sORG_JSON)-2);
end;
/
show errors;
create or replace public synonym UDO_P_GET_PSORG for UDO_P_GET_PSORG;
grant execute on UDO_P_GET_PSORG to public;
